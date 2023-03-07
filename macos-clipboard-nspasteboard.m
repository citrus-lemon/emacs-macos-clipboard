#include "emacs-module.h"
#import <Cocoa/Cocoa.h>
#import <Foundation/Foundation.h>
#include <string.h>

int plugin_is_GPL_compatible;

#define Qnil env->intern(env, "nil")

static emacs_value extract_pasteboard(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data) {
  emacs_value Qcons = env->intern(env, "cons");
  emacs_value result = Qnil;

  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
  for (NSPasteboardItem *item in [pasteboard pasteboardItems]) {
    emacs_value Sitem = Qnil;
    for (NSPasteboardType type in [item types]) {
      emacs_value Stype =
          env->make_string(env, [type UTF8String], strlen([type UTF8String]));

      emacs_value Sdata = Qnil;
      NSString *str = [item stringForType:type];
      if (str) {
        Sdata =
            env->make_string(env, [str UTF8String], strlen([str UTF8String]));
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
    emacs_value args[] = {Sitem, result};
    result = env->funcall(env, Qcons, 2, args);
  }
  [pool release];
  return result;
}

int emacs_module_init(struct emacs_runtime *runtime) {
  if (runtime->size < sizeof(*runtime))
    return 1;

  emacs_env *env = runtime->get_environment(runtime);

  emacs_value Qsym = env->intern(env, "macos-clipboard-extract-pasteboard");
  emacs_value Sfun = env->make_function(env, 0, 0, extract_pasteboard, "", nil);
  emacs_value aa[] = {Qsym, Sfun};
  env->funcall(env, env->intern(env, "fset"), 2, aa);

  emacs_value Qfeat = env->intern(env, "macos-clipboard-nspasteboard");
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};
  env->funcall(env, Qprovide, 1, args);
  return 0;
}
