#include <stdbool.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include <unistd.h>

#include "rime-api.h"

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi* api;
  bool firstRun;
} EmacsRime;

void* EmacsRimeCreate();
void EmacsRimeStart(EmacsRime*, bool);


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
}

void EmacsRimeTest(EmacsRime* rime) {
  char* schema = (char *) malloc(4096);
  bool result = rime->api->get_current_schema(rime->session_id, schema, 4096);
  printf("success: %d, current_schema is %s\n", result, schema);
  free(schema);
}

int main() {
  printf("Test Calling Rime API\n");
  EmacsRime* rime = EmacsRimeCreate();
  printf("Successfully created rime session\n");
  EmacsRimeTest(rime);
  while(true) {
    sleep(1);
  }
}
