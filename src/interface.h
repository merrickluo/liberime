#include <stdarg.h>

#include "emacs-module.h"

#ifndef INTERFACE_H
#define INTERFACE_H

extern emacs_value em_nil, em_stringp, em_t;

// Reference types
extern emacs_value em_direct, em_symbolic;

/**
 * Initialize the libegit2-emacs interface.
 * This function should only be called once.
 */
void em_init(emacs_env *env);

/**
 * Signal a wrong-type-argument error if PREDICATE does not apply to ARG.
 * @param env The active Emacs environment.
 * @param predicate The predicate.
 * @param arg The argument.
 * @return True iff an error was signaled.
 */
bool em_assert(emacs_env *env, emacs_value predicate, emacs_value arg);

/**
 * Signal an error originating form libgit2.
 * @param env The active Emacs environment.
 * @param _klass The error code.
 * @param _msg The error message.
 */
void em_signal_rimeerr(emacs_env *env, int _klass, const char *_msg);

/**
 * Signal a wrong-type-argument error.
 * @param env The active Emacs environment.
 * @param expected Symbol describing the expected type.
 * @param actual Emacs value that does not have the expected type.
 */
void em_signal_wrong_type(emacs_env *env, emacs_value expected,
                          emacs_value actual);

/**
 * Signal a wrong-value-argument error.
 * @param env The active Emacs environment.
 * @param actual Emacs value that does not have the expected value.
 */
void em_signal_wrong_value(emacs_env *env, emacs_value actual);

/**
 * Return a string from an emacs_value.
 * Caller is responsible for ensuring that the value is a string, and to free
 * the returned pointer.
 * @param env The active Emacs environment.
 * @param arg Emacs value representing a string.
 * @return The string (owned pointer).
 */
char *em_get_string(emacs_env *env, emacs_value arg);

/**
 * Call (cons car cdr) in Emacs.
 * @param env The active Emacs environment.
 * @param car The car.
 * @param cdr The cdr.
 * @return The cons cell.
 */
emacs_value em_cons(emacs_env *env, emacs_value car, emacs_value cdr);

/**
 * Call (define-error SYMBOL MSG) in Emacs.
 * @param env The active Emacs environment.
 * @param car The error symbol.
 * @param cdr The error description.
 */
void em_define_error(emacs_env *env, emacs_value symbol, const char *msg);

/**
 * Define a function in Emacs, using defalias.
 * @param env The active Emacs environment.
 * @param name Symbol name of the desired function.
 * @param func Function to bind.
 */
void em_defun(emacs_env *env, const char *name, emacs_value func);

/**
 * Call (expand-file-name PATH) in Emacs.
 * @param env The active Emacs environment.
 * @param path The path to expand.
 */
emacs_value em_expand_file_name(emacs_env *env, emacs_value path);

/**
 * Provide a feature to Emacs.
 * @param env The active Emacs environment.
 * @param name Symbol name of the feature to provide.
 */
void em_provide(emacs_env *env, const char *feature);

/**
 * Check if a value is a user pointer.
 * @param env The active Emacs environment.
 * @param val Value to check.
 * @return True iff val is a user pointer.
 */
bool em_user_ptrp(emacs_env *env, emacs_value val);

/**
 * make a Emacs list with array
 * @param env the active Emacs enviroment.
 * @param size size of the array.
 * @param array pointer.
 * @return emacs_value represent the list.
 */
emacs_value em_list(emacs_env *env, ptrdiff_t size, emacs_value *array);

/**
 * propertize the value
 * @param env the active Emacs enviroment.
 * @param target the propertize target.
 * @param key property key.
 * @param value property value.
 * @return propertized value of param value.
 */
emacs_value em_propertize(emacs_env *env, emacs_value target, const char *key,
                          emacs_value value);
/**
 * make a Emacs symbol with string.
 * @param env the active Emacs enviroment.
 * @param str string that to be a symbol.
 * @return emacs_value represents a symbol.
 */
emacs_value em_symbol(emacs_env *env, const char *str);

/**
 * make a Emacs string with string, always copy it, so it's safe to free *src.
 * @param env the active Emacs enviroment.
 * @param str string that to be convert to emacs string.
 * @return emacs_value represents a emacs string.
 */
emacs_value em_string(emacs_env *env, char *str);

#endif /* INTERFACE_H */
