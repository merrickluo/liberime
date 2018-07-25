#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "emacs-module.h"
#include "interface.h"
#include "liberime.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;


int emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  emacs_env *env = ert->get_environment (ert);

  em_init(env);

  liberime_init(env);

  em_provide (env, "liberime");

  /* loaded successfully */
  return 0;
}
