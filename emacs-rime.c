#include <stdbool.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>

#include "rime-api.h"
#include "emacs-rime.h"

void EmacsRimeNotificationHandler(void *context,
                                  RimeSessionId session_id,
                                  const char* message_type,
                                  const char* message_value) {
  // TODO send message to emacs
  printf("notification: %s: %s\n", message_type, message_value);
}

void* EmacsRimeCreate() {
  EmacsRime* rime = (EmacsRime*) malloc(sizeof(EmacsRime));
  rime->api = rime_get_api();
  rime->firstRun = true;

  if (!rime->api) {
    free(rime);
    return NULL;
  }

  EmacsRimeStart(rime, false);

  return rime;
}

void EmacsRimeStart(EmacsRime* rime, bool fullcheck) {
  RIME_STRUCT(RimeTraits, emacs_rime_traits);
  emacs_rime_traits.shared_data_dir = "/usr/share/rime-data";
  emacs_rime_traits.app_name = "rime.emacs";
  emacs_rime_traits.user_data_dir = "/home/merrick/.emacs.d/rime";
  emacs_rime_traits.distribution_name = "Rime";
  emacs_rime_traits.distribution_code_name = "emacs-rime";
  emacs_rime_traits.distribution_version = "0.0.1";
  if (rime->firstRun) {
    rime->api->setup(&emacs_rime_traits);
    rime->firstRun = false;
  }

  rime->api->initialize(&emacs_rime_traits);
  rime->api->set_notification_handler(EmacsRimeNotificationHandler, rime);
  rime->api->start_maintenance(true);

  rime->session_id = rime->api->create_session();
  printf("created rime session: %ld\n", rime->session_id);
}

char* EmacsRimeGetCandWords(EmacsRime *rime) {
  RIME_STRUCT(RimeCommit, commit);
  if (rime->api->get_commit(rime->session_id, &commit)) {
    printf("commited: %s\n", commit.text);
    return commit.text;
  }

  RIME_STRUCT(RimeContext, context);
  if (!rime->api->get_context(rime->session_id, &context)) {
    printf("no context? \n");
    return NULL;
  }

  if (context.composition.length == 0) {
    printf("no candidates\n");
    rime->api->free_context(&context);
    return NULL;
  }

  printf("commit_text_preview: %s\n", context.commit_text_preview);

  char *preview = malloc(strlen(context.commit_text_preview));
  strcpy(preview, context.commit_text_preview);

  printf("composition: \n");
  printf("sel_start: %d\nsel_end: %d\npreedit:%s\ncursor_pos:%d\nlength:%d\n",
         context.composition.sel_start,
         context.composition.sel_end,
         context.composition.preedit,
         context.composition.cursor_pos,
         context.composition.length);

  for (int i = 0; i < context.menu.num_candidates; i++) {
    printf("%s\n", context.menu.candidates[i].text);
  }

  return preview;
}

char *EmacsRimeSearch(EmacsRime *rime, char* pinyin) {
  if (!rime->api->find_session(rime->session_id)) {
    rime->session_id = rime->api->create_session();
    if (!rime->session_id) {
      printf("cannot create rime session\n");
      return NULL;
    }
  }

  rime->api->clear_composition(rime->session_id);
  rime->api->simulate_key_sequence(rime->session_id, pinyin);
  return EmacsRimeGetCandWords(rime);
}

void EmacsRimeGetStatus(EmacsRime *rime) {
  RIME_STRUCT(RimeStatus, status);
  if (rime->api->get_status(rime->session_id, &status)) {
    printf("status is %s\n", status.schema_name);
  }
}

char* EmacsRimeTest(EmacsRime* rime) {
  if (!rime->api->find_session(rime->session_id)) {
    rime->session_id = rime->api->create_session();
    if (!rime->session_id) {
      printf("cannot create rime session\n");
      return NULL;
    }
  }
  rime->api->process_key(rime->session_id, 0x77, 0);
  rime->api->process_key(rime->session_id, 0x6f, 0);
  rime->api->process_key(rime->session_id, 0x64, 0);
  rime->api->process_key(rime->session_id, 0x3d, 0);
  // rime->api->process_key(rime->session_id, 0x32, 0);
  // rime->api->simulate_key_sequence(rime->session_id, "wode");
  // rime->api->process_key(rime->session_id, 187, 0);
  return EmacsRimeGetCandWords(rime);
}

int main() {
  printf("Test Calling Rime API\n");
  EmacsRime* rime = EmacsRimeCreate();
  printf("Successfully created rime session\n");
  while(true) {
    sleep(1);
    // EmacsRimeGetStatus(rime);
    EmacsRimeTest(rime);
  }
}
