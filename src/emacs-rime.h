#include <stdbool.h>

#include "rime-api.h"

#ifndef EMACS_RIME_H
#define EMACS_RIME_H

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi* api;
  bool firstRun;
} EmacsRime;


void* EmacsRimeCreate();
void EmacsRimeStart(EmacsRime*, bool);
char* EmacsRimeTest(EmacsRime*);
bool EmacsRimeProcessKey(EmacsRime *rime, int keycode, int mask);
char* EmacsRimeGetCandWords(EmacsRime *rime);
char *EmacsRimeSearch(EmacsRime *rime, char* pinyin);

#endif
