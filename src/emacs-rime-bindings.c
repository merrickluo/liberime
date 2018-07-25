#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "emacs-module.h"
#include "emacs-module-helpers.h"
#include "emacs-rime.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

/* New emacs lisp function. All function exposed to Emacs must have this prototype. */
static emacs_value
EmacsRimeTestBind (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  EmacsRime* rime = (EmacsRime *) data;
  char* cand = EmacsRimeTest(rime);
  return env->make_string(env, cand, strlen(cand));
}

static emacs_value
EmacsRimeProcessKeyBind (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime* rime = (EmacsRime*) data;
  int keycode = extract_integer(env, args[0]);
  int mask = extract_integer(env, args[1]);
  if (EmacsRimeProcessKey(rime, keycode, mask)) {
    return env->make_integer(env, 0);
  }
  return env->make_integer(env, 1);
}

static emacs_value
EmacsRimeGetCandWordsBind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime* rime = (EmacsRime*) data;
  char* cand = EmacsRimeGetCandWords(rime);
  return env->make_string(env, cand, strlen(cand));
}

static emacs_value
EmacsRimeSearchBind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  EmacsRime* rime = (EmacsRime *) data;

  // first get string size
  ptrdiff_t size = 0;
  if (!env->copy_string_contents(env, args[0], NULL, &size)) {
    return env->make_string(env, "", 0);
  }

  char* pinyin = malloc(size);

  if (env->copy_string_contents(env, args[0], pinyin, &size)) {
    printf("get pinyin input from emacs: %s\n", pinyin);
    char* cand = EmacsRimeSearch(rime, pinyin);
    printf("simple result is %s\n", cand);
    return env->make_string(env, cand, strlen(cand));
  }
  printf("failed to parse arguments\n");
  return env->make_string(env, "", 0);
}


int emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  emacs_env *env = ert->get_environment (ert);

  EmacsRime* rime = EmacsRimeCreate();
  DEFUN("rime-test", EmacsRimeTestBind, 0, 1, "test", rime);
  DEFUN("rime-search", EmacsRimeSearchBind, 1, 1, "search", rime);
  DEFUN("rime-process-key", EmacsRimeProcessKeyBind, 2, 2, "process key", rime);
  DEFUN("rime-get-candidates", EmacsRimeGetCandWordsBind, 0, 0, "get candidates", rime);

  provide (env, "liberime");

  /* loaded successfully */
  return 0;
}
