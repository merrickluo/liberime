#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"

#define GLOBREF(val) env->make_global_ref(env, (val))
#define INTERN(val) env->intern(env, (val))

#define MAX_STRLEN 1024

// We store some globa references to emacs objects, mostly symbols,
// so that we don't have to waste time calling intern later on.

emacs_value em_nil, em_stringp, em_t;

// Reference types
emacs_value em_direct, em_symbolic;

// Symbols that are only reachable from within this file.
static emacs_value _cons, _defalias, _define_error, _expand_file_name, _rimeerr,
    _not_implemented, _provide, _user_ptrp, _vector, _wrong_type_argument,
    _wrong_value_argument, _list, _propertize;

void em_init(emacs_env *env) {
  em_nil = GLOBREF(INTERN("nil"));
  em_stringp = GLOBREF(INTERN("stringp"));
  em_t = GLOBREF(INTERN("t"));

  em_direct = GLOBREF(INTERN("direct"));
  em_symbolic = GLOBREF(INTERN("symbolic"));

  _list = GLOBREF(INTERN("list"));
  _cons = GLOBREF(INTERN("cons"));
  _defalias = GLOBREF(INTERN("defalias"));
  _define_error = GLOBREF(INTERN("define-error"));
  _expand_file_name = GLOBREF(INTERN("expand-file-name"));
  _rimeerr = GLOBREF(INTERN("rimeerr"));
  _not_implemented = GLOBREF(INTERN("not-implemented"));
  _provide = GLOBREF(INTERN("provide"));
  _user_ptrp = GLOBREF(INTERN("user-ptrp"));
  _vector = GLOBREF(INTERN("vector"));
  _wrong_type_argument = GLOBREF(INTERN("wrong-type-argument"));
  _wrong_value_argument = GLOBREF(INTERN("wrong-value-argument"));
  _propertize = GLOBREF(INTERN("propertize"));

  em_define_error(env, _rimeerr, "Rime error");
  em_define_error(env, _not_implemented, "Not implemented");
  em_define_error(env, _wrong_value_argument, "Wrong argument value passed");
}

/**
 * Call an Emacs function without error checking.
 * @param env The active Emacs environment.
 * @param func The function to call.
 * @param nargs The number of arguments that follow.
 * @return The function return value.
 */
static emacs_value em_funcall(emacs_env *env, emacs_value func, ptrdiff_t nargs,
                              ...) {
  emacs_value args[nargs];

  va_list vargs;
  va_start(vargs, nargs);
  for (ptrdiff_t i = 0; i < nargs; i++)
    args[i] = va_arg(vargs, emacs_value);
  va_end(vargs);

  return env->funcall(env, func, nargs, args);
}

bool em_assert(emacs_env *env, emacs_value predicate, emacs_value arg) {
  bool cond = env->is_not_nil(env, em_funcall(env, predicate, 1, arg));
  if (!cond)
    em_signal_wrong_type(env, predicate, arg);
  return cond;
}

void em_signal_rimeerr(emacs_env *env, int _klass, const char *_msg) {
  emacs_value klass = env->make_integer(env, _klass);
  emacs_value msg = env->make_string(env, _msg, strlen(_msg));
  env->non_local_exit_signal(env, _rimeerr,
                             em_cons(env, klass, em_cons(env, msg, em_nil)));
}

void em_signal_wrong_type(emacs_env *env, emacs_value expected,
                          emacs_value actual) {
  env->non_local_exit_signal(
      env, _wrong_type_argument,
      em_cons(env, expected, em_cons(env, actual, em_nil)));
}

void em_signal_wrong_value(emacs_env *env, emacs_value actual) {
  env->non_local_exit_signal(env, _wrong_value_argument, actual);
}

char *em_get_string(emacs_env *env, emacs_value arg) {
  if (arg == NULL) {
    return NULL;
  } else {
    ptrdiff_t size;
    env->copy_string_contents(env, arg, NULL, &size);

    char *buf = (char *)malloc(size * sizeof(char));
    env->copy_string_contents(env, arg, buf, &size);

    return buf;
  }
}

emacs_value em_cons(emacs_env *env, emacs_value car, emacs_value cdr) {
  return em_funcall(env, _cons, 2, car, cdr);
}

void em_define_error(emacs_env *env, emacs_value symbol, const char *msg) {
  em_funcall(env, _define_error, 2, symbol,
             env->make_string(env, msg, strlen(msg)));
}

void em_defun(emacs_env *env, const char *name, emacs_value func) {
  em_funcall(env, _defalias, 2, INTERN(name), func);
}

emacs_value em_expand_file_name(emacs_env *env, emacs_value path) {
  return em_funcall(env, _expand_file_name, 1, path);
}

void em_provide(emacs_env *env, const char *feature) {
  em_funcall(env, _provide, 1, INTERN(feature));
}

bool em_user_ptrp(emacs_env *env, emacs_value val) {
  return env->is_not_nil(env, em_funcall(env, _user_ptrp, 1, val));
}

emacs_value em_list(emacs_env *env, ptrdiff_t array_size, emacs_value *array) {
  return env->funcall(env, _list, array_size, array);
}

emacs_value em_propertize(emacs_env *env, emacs_value target, const char *key,
                          emacs_value value) {
  return em_funcall(env, _propertize, 3, target, INTERN(key), value);
}

emacs_value em_string(emacs_env *env, char *str) {
  // always copy string
  if (str) {
    size_t size = strnlen(str, MAX_STRLEN);
    char *new_str = malloc(size + 1);
    strncpy(new_str, str, size);
    return env->make_string(env, new_str, size);
  } else {
    return em_nil;
  }
}

emacs_value em_symbol(emacs_env *env, const char *str) {
  return INTERN(str);
}
