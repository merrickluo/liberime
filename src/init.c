#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "emacs-module.h"
#include "interface.h"
#include "liberime-core.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;

int emacs_module_init(struct emacs_runtime *ert) EMACS_NOEXCEPT {
  emacs_env *env = ert->get_environment(ert);

  em_init(env);

  liberime_init(env);

  em_provide(env, "liberime-core");

  /* loaded successfully */
  return 0;
}
