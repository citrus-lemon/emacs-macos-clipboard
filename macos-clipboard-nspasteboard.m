#include "emacs-module.h"
#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>
#include <string.h>

int plugin_is_GPL_compatible;

#define Qnil env->intern(env, "nil")
#define DEFUN(name, func, min_arity, max_arity, docstring)                     \
  {                                                                            \
    emacs_value Qsym = env->intern(env, name);                                 \
    emacs_value Sfun =                                                         \
        env->make_function(env, min_arity, max_arity, func, docstring, nil);   \
    emacs_value args[] = {Qsym, Sfun};                                         \
    env->funcall(env, env->intern(env, "fset"), 2, args);                      \
  }
#define PROVIDE(feat)                                                          \
  {                                                                            \
    emacs_value Qfeat = env->intern(env, feat);                                \
    emacs_value Qprovide = env->intern(env, "provide");                        \
    emacs_value args[] = {Qfeat};                                              \
    env->funcall(env, Qprovide, 1, args);                                      \
  }

static emacs_value extract_pasteboard(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[],
                                      void *data) EMACS_NOEXCEPT {
  emacs_value Qcons = env->intern(env, "cons");
  emacs_value Qnreverse = env->intern(env, "nreverse");
  emacs_value result = Qnil;

  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  @try {
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
    NSArray<NSPasteboardItem *> *pasteboardItems = [pasteboard pasteboardItems];

    if (pasteboardItems) {
      for (NSPasteboardItem *item in pasteboardItems) {
        emacs_value Sitem = Qnil;

        NSArray<NSPasteboardType> *types = [item types];

        if (!types)
          continue;

        for (NSPasteboardType type in types) {
          emacs_value Stype = env->make_string(env, [type UTF8String],
                                               strlen([type UTF8String]));

          emacs_value Sdata = Qnil;
          NSString *str = [item stringForType:type];
          if (str) {
            Sdata = env->make_string(env, [str UTF8String],
                                     strlen([str UTF8String]));
          } else {
            NSData *data = [item dataForType:type];
            if (data) {
              Sdata = env->make_unibyte_string(env, (const char *)[data bytes],
                                               [data length]);
            }
          }

          emacs_value c_args[] = {Stype, Sdata};
          emacs_value Sentry = env->funcall(env, Qcons, 2, c_args);

          emacs_value args[] = {Sentry, Sitem};
          Sitem = env->funcall(env, Qcons, 2, args);
        }
        Sitem = env->funcall(env, Qnreverse, 1, &Sitem);

        emacs_value args[] = {Sitem, result};
        result = env->funcall(env, Qcons, 2, args);
      }
    }
    result = env->funcall(env, Qnreverse, 1, &result);
  } @catch (NSException *exception) {
    NSLog(@"[ERROR] %@", exception);

    NSString *reason = [exception reason];
    if (!reason) {
      reason = @"";
    }

    emacs_value Sreason =
        env->make_string(env, [reason UTF8String], strlen([reason UTF8String]));

    env->funcall(env, env->intern(env, "error"), 1, &Sreason);
  } @finally {
    [pool release];
  }

  return result;
}

static emacs_value set_string_pasteboard(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[],
                                         void *data) EMACS_NOEXCEPT {
  emacs_value str = args[0];
  emacs_value type = nargs > 1 ? args[1] : Qnil;
  NSString *ptype;

  if (env->is_not_nil(env, type)) {
    char buffer[128];
    ptrdiff_t buffer_size = sizeof(buffer);
    if (!env->copy_string_contents(env, type, buffer, &buffer_size)) {
      // error no too long type name
      return Qnil;
    }
    ptype = [NSString stringWithUTF8String:buffer];
  } else {
    ptype = @"public.utf8-plain-text";
  }

  NSString *value;
  {
    char *buffer = NULL;
    ptrdiff_t buffer_size = 0;
    env->copy_string_contents(env, str, buffer, &buffer_size);
    buffer = malloc(buffer_size);
    env->copy_string_contents(env, str, buffer, &buffer_size);
    value = [NSString stringWithUTF8String:buffer];
  }

  NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
  [pasteboard setString:value forType:ptype];

  return Qnil;
}

int emacs_module_init(struct emacs_runtime *runtime) EMACS_NOEXCEPT {
  if (runtime->size < sizeof(*runtime))
    return 1;
  emacs_env *env = runtime->get_environment(runtime);

  DEFUN("macos-clipboard-extract-pasteboard", extract_pasteboard, 0, 0, "");
  DEFUN("macos-clipboard-set-string", set_string_pasteboard, 1, 2, "");
  PROVIDE("macos-clipboard-nspasteboard");
  return 0;
}
