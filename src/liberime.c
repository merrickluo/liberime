#include <stdbool.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <rime_api.h>

#include <unistd.h>

#include "interface.h"
#include "liberime.h"

#define DEFUN(ename, cname, min_nargs, max_nargs, doc, data)            \
  em_defun(env, ename,                                                  \
           env->make_function(env, min_nargs, max_nargs, cname, doc, data))


typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi* api;
  bool firstRun;
} EmacsRime;

typedef struct _EmacsRimeCandidates {
  size_t size;
  char** candidates;
} EmacsRimeCandidates;

void notification_handler(void *context,
                          RimeSessionId session_id,
                          const char* message_type,
                          const char* message_value) {
  // TODO send message to emacs
  printf("notification: %s: %s\n", message_type, message_value);
}

static bool ensure_session(EmacsRime *rime) {
  if (!rime->api->find_session(rime->session_id)) {
    rime->session_id = rime->api->create_session();
    if (!rime->session_id) {
      printf("cannot create rime session\n");
      return false;
    }
  }
  return true;
}

EmacsRimeCandidates get_candidates(EmacsRime *rime) {
  EmacsRimeCandidates c = { .size = 0,
                            .candidates = malloc(4096 * sizeof(char*)) };

  RIME_STRUCT(RimeContext, context);
  if (!rime->api->get_context(rime->session_id, &context)) {
    printf("no context? \n");
    return c;
  }

  if (context.composition.length == 0) {
    printf("no candidates\n");
    rime->api->free_context(&context);
    return c;
  }

  RimeCandidateListIterator iterator = {0};
  if (rime->api->candidate_list_begin(rime->session_id, &iterator)) {
    while (rime->api->candidate_list_next(&iterator)) {
      c.candidates[c.size] = malloc(strlen(iterator.candidate.text));
      strcpy(c.candidates[c.size], iterator.candidate.text);
      c.size += 1;
      if (c.size > 4000) {
        break;
      }
    }
    rime->api->candidate_list_end(&iterator);
  }

  rime->api->free_context(&context);

  return c;
}

// bindings
static emacs_value
liberime_start(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void* data) {
  EmacsRime *rime = (EmacsRime*) data;

  char* shared_data_dir = em_get_string(env, args[0]);
  char* user_data_dir = em_get_string(env, args[1]);

  RIME_STRUCT(RimeTraits, emacs_rime_traits);

  emacs_rime_traits.shared_data_dir = shared_data_dir;
  emacs_rime_traits.app_name = "rime.emacs";
  emacs_rime_traits.user_data_dir = user_data_dir;
  emacs_rime_traits.distribution_name = "Rime";
  emacs_rime_traits.distribution_code_name = "emacs-rime";
  emacs_rime_traits.distribution_version = "0.1.0";
  if (rime->firstRun) {
    rime->api->setup(&emacs_rime_traits);
    rime->firstRun = false;
  }

  rime->api->initialize(&emacs_rime_traits);
  rime->api->set_notification_handler(notification_handler, rime);
  rime->api->start_maintenance(true);

  rime->session_id = rime->api->create_session();

  return em_t;
}

emacs_value
liberime_search(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;
  if (nargs < 1) {
    // TODO report error
    return NULL;
  }
  char* pinyin = em_get_string(env, args[0]);

  if (!rime->api->find_session(rime->session_id)) {
    rime->session_id = rime->api->create_session();
    if (!rime->session_id) {
      printf("cannot create rime session\n");
      return NULL;
    }
  }

  rime->api->clear_composition(rime->session_id);
  rime->api->simulate_key_sequence(rime->session_id, pinyin);

  EmacsRimeCandidates candidates = get_candidates(rime);

  printf("find candidates size: %ld\n", candidates.size);
  emacs_value* array = malloc(sizeof(emacs_value) * candidates.size);

  for (int i = 0; i < candidates.size; i++) {
    const char *value = candidates.candidates[i];
    array[i] = env->make_string(env, value, strlen(value));
  }

  emacs_value list = env->intern(env, "list");
  emacs_value result = env->funcall(env, list, candidates.size, array);

  free(candidates.candidates);
  free(array);
  free(pinyin);

  return result;
}


void liberime_init(emacs_env* env) {
  EmacsRime* rime = (EmacsRime*) malloc(sizeof(EmacsRime));

  rime->api = rime_get_api();
  rime->firstRun = true; // not used yet

  if (!rime->api) {
    free(rime);
    // TODO report error
    return;
  }

  DEFUN("rime-search", liberime_search, 1, 1, "convert pinyin to candidates", rime);
  DEFUN("rime-start", liberime_start, 2, 2, "start rime session", rime);
}
