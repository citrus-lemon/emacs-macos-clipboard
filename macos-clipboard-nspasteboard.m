#include "emacs-module.h"
#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>
#include <string.h>

int plugin_is_GPL_compatible;

#define DEFUN(name, func, min_arity, max_arity, docstring)                 \
  do {                                                                     \
    emacs_value Qsym = env->intern(env, name);                             \
    emacs_value Sfun = env->make_function(env, min_arity, max_arity,       \
                                          func, docstring, NULL);          \
    emacs_value args__[] = { Qsym, Sfun };                                 \
    env->funcall(env, env->intern(env, "fset"), 2, args__);                \
  } while (0)

#define PROVIDE(feat)                                                      \
  do {                                                                     \
    emacs_value Qfeat = env->intern(env, feat);                            \
    emacs_value args__[] = { Qfeat };                                      \
    env->funcall(env, env->intern(env, "provide"), 1, args__);             \
  } while (0)

/* ----- Utilities ----- */

static char *copy_lisp_string_utf8(emacs_env *env, emacs_value s, ptrdiff_t *out_len /* may be NULL */) {
  ptrdiff_t n = 0;
  if (!env->copy_string_contents(env, s, NULL, &n)) return NULL;
  char *buf = (char *)malloc((size_t)n);
  if (!buf) return NULL;
  if (!env->copy_string_contents(env, s, buf, &n)) { free(buf); return NULL; }
  if (out_len) *out_len = n;  /* n includes the trailing NUL */
  return buf;
}

static BOOL lisp_is_nil(emacs_env *env, emacs_value v) {
  return !env->is_not_nil(env, v);
}

static emacs_value make_lisp_string(emacs_env *env, NSString *s) {
  if (!s) return env->intern(env, "nil");
  const char *u = [s UTF8String];
  if (!u) return env->intern(env, "nil");
  return env->make_string(env, u, (ptrdiff_t)strlen(u));
}

/* Convert Lisp list of strings to NSArray<NSString *> *.
   Non-strings are skipped silently. */
static NSArray<NSString *> *list_to_nsstrings(emacs_env *env, emacs_value list) {
  NSMutableArray<NSString *> *arr = [NSMutableArray array];
  emacs_value Qcar = env->intern(env, "car");
  emacs_value Qcdr = env->intern(env, "cdr");

  while (env->is_not_nil(env, list)) {
    emacs_value car = env->funcall(env, Qcar, 1, &list);
    ptrdiff_t len = 0;
    char *utf8 = copy_lisp_string_utf8(env, car, &len);
    if (utf8) {
      NSString *s = [[NSString alloc] initWithBytes:utf8 length:(len ? (len - 1) : 0)
                                           encoding:NSUTF8StringEncoding];
      if (s) [arr addObject:s];
      free(utf8);
    }
    list = env->funcall(env, Qcdr, 1, &list);
  }
  return arr;
}

/* Create unibyte Lisp string from NSData (binary) */
static emacs_value make_unibyte_from_nsdata(emacs_env *env, NSData *data) {
  if (!data || data.length == 0) return env->intern(env, "nil");
  return env->make_unibyte_string(env, (const char *)data.bytes, (ptrdiff_t)data.length);
}

/* Convert a Lisp (possibly unibyte) string back to original raw bytes.
   We rely on Emacs providing UTF-8 via copy_string_contents, then decode UTF-8
   to NSString and re-encode as ISO-8859-1 to recover 0..255 bytes. */
static NSData *nsdata_from_lisp_unibyte_string(emacs_env *env, emacs_value s) {
  ptrdiff_t n = 0;
  char *utf8 = copy_lisp_string_utf8(env, s, &n);
  if (!utf8 || n == 0) { if (utf8) free(utf8); return nil; }
  NSString *str = [[NSString alloc] initWithBytes:utf8 length:(n - 1) encoding:NSUTF8StringEncoding];
  free(utf8);
  if (!str) return nil;
  NSData *bytes = [str dataUsingEncoding:NSISOLatin1StringEncoding allowLossyConversion:NO];
  return bytes;
}

/* Resolve file:///.file/id=... or normal file:// URL to a POSIX path string. */
static NSString *resolve_file_url_to_path(NSString *urlString) {
  if (!urlString) return nil;
  NSURL *url = [NSURL URLWithString:urlString];
  if (!url || !url.isFileURL) return nil;

  NSString *path = url.path;
  if (path.length > 0) return path;

  // Try resource value resolution for file-reference URLs.
  NSError *err = nil;
  if ([url getResourceValue:&path forKey:NSURLPathKey error:&err] && path.length > 0) {
    return path;
  }
  return nil;
}

/* ----- Functions ----- */

/* (macos-clipboard--extract-pasteboard) -> list of items; each item is list of (TYPE . DATA) */
static emacs_value F_extract_pasteboard(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  emacs_value Qnil = env->intern(env, "nil");
  emacs_value Qcons = env->intern(env, "cons");
  emacs_value Qnreverse = env->intern(env, "nreverse");
  emacs_value result = Qnil;
  emacs_value result_prot = env->make_global_ref(env, result);

  @autoreleasepool {
    @try {
      NSArray<NSPasteboardItem *> *items = [[NSPasteboard generalPasteboard] pasteboardItems];
      if (items) {
        for (NSPasteboardItem *item in items) {
          emacs_value Sitem = Qnil;
          emacs_value Sitem_prot = env->make_global_ref(env, Sitem);

          NSArray<NSPasteboardType> *types = [item types];
          if (!types) { env->free_global_ref(env, Sitem_prot); continue; }

          for (NSPasteboardType type in types) {
            const char *type_c = [type UTF8String];
            if (!type_c) continue;
            emacs_value Stype = env->make_string(env, type_c, (ptrdiff_t)strlen(type_c));

            emacs_value Sdata = Qnil;
            NSString *str = [item stringForType:type];
            if (str) {
              Sdata = make_lisp_string(env, str);
            } else {
              NSData *bin = [item dataForType:type];
              if (bin && bin.length > 0) {
                Sdata = make_unibyte_from_nsdata(env, bin);
              }
            }

            emacs_value inner_args[] = { Stype, Sdata };
            emacs_value Sentry = env->funcall(env, Qcons, 2, inner_args);

            emacs_value cons_args[] = { Sentry, Sitem };
            emacs_value new_Sitem = env->funcall(env, Qcons, 2, cons_args);

            env->free_global_ref(env, Sitem_prot);
            Sitem = new_Sitem;
            Sitem_prot = env->make_global_ref(env, Sitem);
          }

          emacs_value item_rev = env->funcall(env, Qnreverse, 1, &Sitem);
          env->free_global_ref(env, Sitem_prot);

          emacs_value cons_args2[] = { item_rev, result };
          emacs_value new_result = env->funcall(env, Qcons, 2, cons_args2);
          env->free_global_ref(env, result_prot);
          result = new_result;
          result_prot = env->make_global_ref(env, result);
        }
      }

      emacs_value final_rev = env->funcall(env, Qnreverse, 1, &result);
      env->free_global_ref(env, result_prot);
      result = final_rev;
      result_prot = env->make_global_ref(env, result);

    } @catch (NSException *ex) {
      env->free_global_ref(env, result_prot);
      NSString *reason = ex.reason ?: @"";
      emacs_value msg = make_lisp_string(env, reason);
      emacs_value Qerror = env->intern(env, "error");
      env->funcall(env, Qerror, 1, &msg);
      return Qnil;
    }
  }

  env->free_global_ref(env, result_prot);
  return result;
}

/* (macos-clipboard--extract-pasteboard-with-types TYPES)
   TYPES is a Lisp list of UTI strings. Returns list of items, each a list of (TYPE . DATA) *for present types only*. */
static emacs_value F_extract_with_types(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  if (nargs < 1) return env->intern(env, "nil");

  NSArray<NSString *> *wanted = list_to_nsstrings(env, args[0]);

  emacs_value Qnil = env->intern(env, "nil");
  emacs_value Qcons = env->intern(env, "cons");
  emacs_value Qnreverse = env->intern(env, "nreverse");

  emacs_value result = Qnil;
  emacs_value result_prot = env->make_global_ref(env, result);

  @autoreleasepool {
    @try {
      NSArray<NSPasteboardItem *> *items = [[NSPasteboard generalPasteboard] pasteboardItems];
      if (items) {
        for (NSPasteboardItem *item in items) {
          emacs_value Sitem = Qnil;
          emacs_value Sitem_prot = env->make_global_ref(env, Sitem);

          for (NSString *type in wanted) {
            NSString *maybeStr = [item stringForType:type];
            emacs_value Sdata = Qnil;

            if (maybeStr) {
              Sdata = make_lisp_string(env, maybeStr);
            } else {
              NSData *maybeBin = [item dataForType:type];
              if (maybeBin && maybeBin.length > 0) {
                Sdata = make_unibyte_from_nsdata(env, maybeBin);
              }
            }

            if (!lisp_is_nil(env, Sdata)) {
              const char *type_c = [type UTF8String];
              emacs_value Stype = env->make_string(env, type_c, (ptrdiff_t)strlen(type_c));
              emacs_value pair_args[] = { Stype, Sdata };
              emacs_value Sentry = env->funcall(env, Qcons, 2, pair_args);

              emacs_value cons_args[] = { Sentry, Sitem };
              emacs_value new_Sitem = env->funcall(env, Qcons, 2, cons_args);
              env->free_global_ref(env, Sitem_prot);
              Sitem = new_Sitem;
              Sitem_prot = env->make_global_ref(env, Sitem);
            }
          }

          emacs_value item_rev = env->funcall(env, Qnreverse, 1, &Sitem);
          env->free_global_ref(env, Sitem_prot);

          emacs_value cons_args2[] = { item_rev, result };
          emacs_value new_result = env->funcall(env, Qcons, 2, cons_args2);
          env->free_global_ref(env, result_prot);
          result = new_result;
          result_prot = env->make_global_ref(env, result);
        }
      }

      emacs_value final_rev = env->funcall(env, Qnreverse, 1, &result);
      env->free_global_ref(env, result_prot);
      result = final_rev;
      result_prot = env->make_global_ref(env, result);

    } @catch (NSException *ex) {
      env->free_global_ref(env, result_prot);
      NSString *reason = ex.reason ?: @"";
      emacs_value msg = make_lisp_string(env, reason);
      emacs_value Qerror = env->intern(env, "error");
      env->funcall(env, Qerror, 1, &msg);
      return Qnil;
    }
  }

  env->free_global_ref(env, result_prot);
  return result;
}

/* (macos-clipboard--extract-pasteboard-only-types)
   -> list of items; each item is list of UTI strings */
static emacs_value F_extract_only_types(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  emacs_value Qnil = env->intern(env, "nil");
  emacs_value Qcons = env->intern(env, "cons");
  emacs_value Qnreverse = env->intern(env, "nreverse");

  emacs_value result = Qnil;
  emacs_value result_prot = env->make_global_ref(env, result);

  @autoreleasepool {
    @try {
      NSArray<NSPasteboardItem *> *items = [[NSPasteboard generalPasteboard] pasteboardItems];
      if (items) {
        for (NSPasteboardItem *item in items) {
          emacs_value Sitem = Qnil;
          emacs_value Sitem_prot = env->make_global_ref(env, Sitem);

          NSArray<NSPasteboardType> *types = [item types];
          for (NSPasteboardType t in types) {
            const char *type_c = [t UTF8String];
            if (!type_c) continue;
            emacs_value Stype = env->make_string(env, type_c, (ptrdiff_t)strlen(type_c));
            emacs_value cons_args[] = { Stype, Sitem };
            emacs_value new_Sitem = env->funcall(env, Qcons, 2, cons_args);
            env->free_global_ref(env, Sitem_prot);
            Sitem = new_Sitem;
            Sitem_prot = env->make_global_ref(env, Sitem);
          }

          emacs_value item_rev = env->funcall(env, Qnreverse, 1, &Sitem);
          env->free_global_ref(env, Sitem_prot);

          emacs_value cons_args2[] = { item_rev, result };
          emacs_value new_result = env->funcall(env, Qcons, 2, cons_args2);
          env->free_global_ref(env, result_prot);
          result = new_result;
          result_prot = env->make_global_ref(env, result);
        }
      }

      emacs_value final_rev = env->funcall(env, Qnreverse, 1, &result);
      env->free_global_ref(env, result_prot);
      result = final_rev;
      result_prot = env->make_global_ref(env, result);

    } @catch (NSException *ex) {
      env->free_global_ref(env, result_prot);
      NSString *reason = ex.reason ?: @"";
      emacs_value msg = make_lisp_string(env, reason);
      env->funcall(env, env->intern(env, "error"), 1, &msg);
      return Qnil;
    }
  }

  env->free_global_ref(env, result_prot);
  return result;
}

/* (macos-clipboard--clearPasteboard) */
static emacs_value F_clear_pasteboard(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  @autoreleasepool {
    [[NSPasteboard generalPasteboard] clearContents];
  }
  return env->intern(env, "nil");
}

/* (macos-clipboard--set-string VALUE &optional TYPE) */
static emacs_value F_set_string(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  ptrdiff_t vlen = 0;
  char *value_c = copy_lisp_string_utf8(env, args[0], &vlen);
  if (!value_c) return env->intern(env, "nil");

  NSString *ptype = @"public.utf8-plain-text";
  if (nargs > 1 && env->is_not_nil(env, args[1])) {
    ptrdiff_t tlen = 0;
    char *type_c = copy_lisp_string_utf8(env, args[1], &tlen);
    if (type_c) { ptype = [NSString stringWithUTF8String:type_c]; free(type_c); }
  }

  @autoreleasepool {
    NSString *value = [[NSString alloc] initWithBytes:value_c length:(vlen ? (vlen - 1) : 0)
                                             encoding:NSUTF8StringEncoding];
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    [pb clearContents];
    if (value) [pb setString:value forType:ptype];
  }

  free(value_c);
  return env->intern(env, "nil");
}

/* (macos-clipboard--set-data VALUE TYPE)
   VALUE is a Lisp string containing raw bytes (use string-as-unibyte / string-to-unibyte in ELisp).
   TYPE is a UTI string, e.g., "public.png". */
static emacs_value F_set_data(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  if (nargs < 2) return env->intern(env, "nil");

  NSData *bytes = nsdata_from_lisp_unibyte_string(env, args[0]);
  if (!bytes) return env->intern(env, "nil");

  ptrdiff_t tlen = 0;
  char *type_c = copy_lisp_string_utf8(env, args[1], &tlen);
  if (!type_c) return env->intern(env, "nil");
  NSString *ptype = [NSString stringWithUTF8String:type_c];
  free(type_c);

  @autoreleasepool {
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    [pb clearContents];
    [pb setData:bytes forType:ptype];
  }

  return env->intern(env, "nil");
}

/* (macos-clipboard-resolve-alias-file FILEURLSTRING) -> POSIX path string or original string if unresolved */
static emacs_value F_resolve_alias_file(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) EMACS_NOEXCEPT {
  if (nargs < 1) return env->intern(env, "nil");
  ptrdiff_t slen = 0;
  char *s = copy_lisp_string_utf8(env, args[0], &slen);
  if (!s) return env->intern(env, "nil");

  emacs_value out = env->intern(env, "nil");
  @autoreleasepool {
    NSString *in = [[NSString alloc] initWithBytes:s length:(slen ? (slen - 1) : 0) encoding:NSUTF8StringEncoding];
    NSString *path = resolve_file_url_to_path(in);
    if (path.length > 0) {
      out = make_lisp_string(env, path);
    } else {
      /* Fallback: return original string unchanged */
      out = make_lisp_string(env, in);
    }
  }
  free(s);
  return out;
}

/* ----- Module entry ----- */

int emacs_module_init(struct emacs_runtime *runtime) EMACS_NOEXCEPT {
  if (runtime->size < sizeof(*runtime)) return 1;
  emacs_env *env = runtime->get_environment(runtime);

  DEFUN("macos-clipboard--extract-pasteboard", F_extract_pasteboard, 0, 0,
        "Return the macOS pasteboard contents as a list of items.\n\
Each item is a list of (TYPE . DATA) pairs, where TYPE is a UTI string\n\
and DATA is a Lisp string (text) or an unibyte string (binary).");

  DEFUN("macos-clipboard--extract-pasteboard-with-types", F_extract_with_types, 1, 1,
        "Return the macOS pasteboard contents filtered by TYPES.\n\
TYPES is a list of UTI strings. For each pasteboard item, include only\n\
present types as (TYPE . DATA) pairs. Missing types are skipped.");

  DEFUN("macos-clipboard--extract-pasteboard-only-types", F_extract_only_types, 0, 0,
        "Return a list of items, each being a list of available UTI strings.");

  DEFUN("macos-clipboard--clearPasteboard", F_clear_pasteboard, 0, 0,
        "Clear all contents from the macOS pasteboard.");

  DEFUN("macos-clipboard--set-string", F_set_string, 1, 2,
        "Set the pasteboard text to VALUE.\n\
Optional TYPE is a UTI (default \"public.utf8-plain-text\").");

  DEFUN("macos-clipboard--set-data", F_set_data, 2, 2,
        "Set the pasteboard binary VALUE for UTI TYPE.\n\
VALUE should be an Emacs unibyte string (use `string-to-unibyte').");

  DEFUN("macos-clipboard-resolve-alias-file", F_resolve_alias_file, 1, 1,
        "Resolve a file URL (including private \".file/id=...\" form) to a POSIX path.\n\
Return the original string if resolution fails.");

  PROVIDE("macos-clipboard-nspasteboard");
  return 0;
}
