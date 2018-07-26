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

#define CANDIDATE_MAXSTRLEN 1024

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi* api;
  bool firstRun;
} EmacsRime;

typedef struct _CandidateLinkedList {
  char* value;
  struct _CandidateLinkedList* next;
} CandidateLinkedList;

typedef struct _EmacsRimeCandidates {
  size_t size;
  CandidateLinkedList* list;
} EmacsRimeCandidates;

void notification_handler(void *context,
                          RimeSessionId session_id,
                          const char* message_type,
                          const char* message_value) {
  // TODO send message to emacs
  printf("notification: %s: %s\n", message_type, message_value);
}

// unused for now
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
  EmacsRimeCandidates c = {.size=0, .list=(CandidateLinkedList *)malloc(sizeof(CandidateLinkedList))};

  RimeCandidateListIterator iterator = {0};
  CandidateLinkedList* next = c.list;
  if (rime->api->candidate_list_begin(rime->session_id, &iterator)) {
    while (rime->api->candidate_list_next(&iterator)) {
      c.size += 1;

      next->value = (char *)malloc(CANDIDATE_MAXSTRLEN + 1);
      strncpy(next->value, iterator.candidate.text, strnlen(iterator.candidate.text, CANDIDATE_MAXSTRLEN) + 1);
      next->value[CANDIDATE_MAXSTRLEN] = '\0';
      next->next = (CandidateLinkedList *)malloc(sizeof(CandidateLinkedList));

      next = next->next;
    }
    next->next = NULL;
    rime->api->candidate_list_end(&iterator);
  }

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

  // wait for deploy
  rime->api->join_maintenance_thread();

  rime->session_id = rime->api->create_session();

  return em_t;
}

void free_candidate_list(CandidateLinkedList *list) {
  CandidateLinkedList* next = list;
  while (next) {
    CandidateLinkedList* temp = next;
    next = temp->next;
    // do not free temp->value
    // it seems emacs_env->make_string didn't do copy
    /* if (temp->value) { */
    /*    free(temp->value); */
    /* } */
    free(temp);
  }
}

emacs_value
liberime_search(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime*) data;
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

  printf("%s: find candidates size: %ld\n", pinyin, candidates.size);
  emacs_value* array = malloc(sizeof(emacs_value) * candidates.size);

  CandidateLinkedList *next = candidates.list;
  int i = 0;
  while (next && i < candidates.size) {
    const char *value = next->value;
    array[i++] = env->make_string(env, value, strlen(value));
    next = next->next;
  }
  printf("conveted array size: %d\n", i);

  emacs_value flist = env->intern(env, "list");
  emacs_value result = env->funcall(env, flist, candidates.size, array);

  // free(candidates.candidates);
  free_candidate_list(candidates.list);
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
    em_signal_rimeerr(env, 1, "Cannot get librime api");
    return;
  }

  DEFUN("liberime-search", liberime_search, 1, 1, "convert pinyin to candidates", rime);
  DEFUN("liberime-start", liberime_start, 2, 2, "start rime session", rime);
}
