#include <rime_api.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include "interface.h"
#include "liberime-core.h"

/**
 * Macro that defines a docstring for a function.
 * @param name The function name (without liberime_ prefix).
 * @param args The argument list as visible from Emacs (without parens).
 * @param docstring The rest of the documentation.
 */
#define DOCSTRING(name, args, docstring)                                       \
  const char *liberime_##name##__doc = (docstring "\n\n(fn " args ")")

#define DEFUN(ename, cname, min_nargs, max_nargs)                              \
  em_defun(env, (ename),                                                       \
           env->make_function(env, (min_nargs), (max_nargs), cname,            \
                              liberime_##cname##__doc, rime))

#define CONS_INT(key, integer)                                                 \
  em_cons(env, env->intern(env, key), env->make_integer(env, integer));
#define CONS_STRING(key, str)                                                  \
  em_cons(env, env->intern(env, key), env->make_string(env, str, strlen(str)))
#define CONS_NIL(key) em_cons(env, env->intern(env, key), em_nil)
#define CONS_VALUE(key, value) em_cons(env, env->intern(env, key), value)

#define CANDIDATE_MAXSTRLEN 1024
#define SCHEMA_MAXSTRLEN 1024
#define CONFIG_MAXSTRLEN 1024
#define INPUT_MAXSTRLEN 1024

#define NO_SESSION_ERR                                                         \
  "Cannot connect to librime session, make sure to run liberime-start first."

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi *api;
  bool first_run;
} EmacsRime;

typedef struct _CandidateLinkedList {
  char *text;
  char *comment;
  struct _CandidateLinkedList *next;
} CandidateLinkedList;

typedef struct _EmacsRimeCandidates {
  size_t size;
  CandidateLinkedList *list;
} EmacsRimeCandidates;

void notification_handler(void *context, RimeSessionId session_id,
                          const char *message_type, const char *message_value) {
  /* EmacsRime *rime = (EmacsRime*) context; */
  /* emacs_env *env = rime->EmacsEnv; */
  /* char format[] = "[liberime] %s: %s"; */
  /* emacs_value args[3]; */
  /* args[0] = env->make_string(env, format, strnlen(format, SCHEMA_MAXSTRLEN));
   */
  /* args[1] = env->make_string(env, message_type, strnlen(message_type,
   * SCHEMA_MAXSTRLEN)); */
  /* args[2] = env->make_string(env, message_value, strnlen(message_value,
   * SCHEMA_MAXSTRLEN)); */
  /* env->funcall(env, env->intern (env, "message"), 3, args); */
}

// make sure session exists before operation
static bool _ensure_session(EmacsRime *rime) {
  if (!rime->api->find_session(rime->session_id)) {
    rime->session_id = rime->api->create_session();
    if (!rime->session_id) {
      // printf("cannot create rime session\n");
      return false;
    }
  }
  return true;
}

static char *_copy_string(char *str) {
  if (str) {
    size_t size = strnlen(str, CANDIDATE_MAXSTRLEN);
    char *new_str = malloc(size + 1);
    strncpy(new_str, str, size);
    new_str[size] = '\0';
    return new_str;
  } else {
    return NULL;
  }
}

EmacsRimeCandidates _get_candidates(EmacsRime *rime, size_t limit) {
  EmacsRimeCandidates c = {
      .size = 0,
      .list = (CandidateLinkedList *)malloc(sizeof(CandidateLinkedList))};

  RimeCandidateListIterator iterator = {0};
  CandidateLinkedList *next = c.list;
  if (rime->api->candidate_list_begin(rime->session_id, &iterator)) {
    while (rime->api->candidate_list_next(&iterator) &&
           (limit == 0 || c.size < limit)) {
      c.size += 1;

      next->text = _copy_string(iterator.candidate.text);
      next->comment = _copy_string(iterator.candidate.comment);

      next->next = (CandidateLinkedList *)malloc(sizeof(CandidateLinkedList));

      next = next->next;
    }
    next->next = NULL;
    rime->api->candidate_list_end(&iterator);
  }

  return c;
}

// bindings
DOCSTRING(start, "SHARED_DATA_DIR USER_DATA_DIR", "Start a rime session.");
static emacs_value start(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                         void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  char *shared_data_dir = em_get_string(env, em_expand_file_name(env, args[0]));
  char *user_data_dir = em_get_string(env, em_expand_file_name(env, args[1]));

  RIME_STRUCT(RimeTraits, emacs_rime_traits);

  emacs_rime_traits.shared_data_dir = shared_data_dir;
  emacs_rime_traits.app_name = "rime.emacs-liberime";
  emacs_rime_traits.user_data_dir = user_data_dir;
  emacs_rime_traits.distribution_name = "Rime";
  emacs_rime_traits.distribution_code_name = "emacs-liberime";
  emacs_rime_traits.distribution_version = "0.1.0";
  if (rime->first_run) {
    rime->api->setup(&emacs_rime_traits);
    rime->first_run = false;
  }

  rime->api->initialize(&emacs_rime_traits);
  rime->api->set_notification_handler(notification_handler, rime);
  rime->api->start_maintenance(true);

  // wait for deploy
  rime->api->join_maintenance_thread();

  rime->session_id = rime->api->create_session();

  return em_t;
}

DOCSTRING(finalize, "", "Finalize librime for redeploy.");
static emacs_value finalize(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (rime->session_id) {
    rime->session_id = 0;
  }
  rime->api->finalize();
  return em_t;
}

void free_candidate_list(CandidateLinkedList *list) {
  CandidateLinkedList *next = list;
  while (next) {
    CandidateLinkedList *temp = next;
    next = temp->next;
    // do not free temp->value
    // it seems emacs_env->make_string didn't do copy
    /* if (temp->value) { */
    /*    free(temp->value); */
    /* } */
    free(temp);
  }
}

DOCSTRING(search, "STRING &optional LIMIT",
          "Input STRING and return LIMIT number candidates.\n"
          "When LIMIT is nil, return all candidates.");
static emacs_value search(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                          void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  char *string = em_get_string(env, args[0]);

  size_t limit = 0;
  if (nargs == 2) {
    if (!env->is_not_nil(env, args[1])) {
      limit = 0;
    } else {
      limit = env->extract_integer(env, args[1]);
      // if limit set to 0 return nil immediately
      if (limit == 0) {
        return em_nil;
      }
    }
  }

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  rime->api->clear_composition(rime->session_id);
  rime->api->simulate_key_sequence(rime->session_id, string);

  EmacsRimeCandidates candidates = _get_candidates(rime, limit);

  // printf("%s: find candidates size: %ld\n", string, candidates.size);
  // return nil if no candidates found
  if (candidates.size == 0) {
    return em_nil;
  }

  emacs_value *array = malloc(sizeof(emacs_value) * candidates.size);

  CandidateLinkedList *next = candidates.list;
  int i = 0;
  while (next && i < candidates.size) {
    emacs_value value = env->make_string(env, next->text, strlen(next->text));
    if (next->comment) {
      emacs_value comment =
          env->make_string(env, next->comment, strlen(next->comment));
      value = em_propertize(env, value, ":comment", comment);
    }
    array[i++] = value;
    next = next->next;
  }
  // printf("conveted array size: %d\n", i);

  emacs_value result = em_list(env, candidates.size, array);

  // free(candidates.candidates);
  free_candidate_list(candidates.list);
  free(array);
  free(string);

  return result;
}

DOCSTRING(get_sync_dir, "", "Get rime sync directory.");
static emacs_value get_sync_dir(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  const char *sync_dir = rime->api->get_sync_dir();
  return env->make_string(env, sync_dir, strlen(sync_dir));
}

DOCSTRING(sync_user_data, "", "Sync rime user data.");
static emacs_value sync_user_data(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  bool result = rime->api->sync_user_data();
  return result ? em_t : em_nil;
}

DOCSTRING(get_schema_list, "", "List all rime schema.");
static emacs_value get_schema_list(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  RimeSchemaList schema_list;
  if (!rime->api->get_schema_list(&schema_list)) {
    em_signal_rimeerr(env, 1, "Get schema list form librime failed.");
    return em_nil;
  }

  emacs_value flist = env->intern(env, "list");
  emacs_value array[schema_list.size];
  for (int i = 0; i < schema_list.size; i++) {
    RimeSchemaListItem item = schema_list.list[i];
    emacs_value pair[2];
    pair[0] = env->make_string(env, item.schema_id,
                               strnlen(item.schema_id, SCHEMA_MAXSTRLEN));
    pair[1] =
        env->make_string(env, item.name, strnlen(item.name, SCHEMA_MAXSTRLEN));

    array[i] = env->funcall(env, flist, 2, pair);
  }

  emacs_value result = env->funcall(env, flist, schema_list.size, array);

  rime->api->free_schema_list(&schema_list);

  return result;
}

DOCSTRING(
    select_schema, "SCHEMA_ID",
    "Select a rime schema.\n"
    "SCHENA_ID should be a value returned from `liberime-get-schema-list'.");
static emacs_value select_schema(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  const char *schema_id = em_get_string(env, args[0]);
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (rime->api->select_schema(rime->session_id, schema_id)) {
    return em_t;
  }
  return em_nil;
}

// input
DOCSTRING(process_key, "KEYCODE &optional MASK",
          "Send KEYCODE to rime session and process it.");
static emacs_value process_key(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  int keycode = env->extract_integer(env, args[0]);
  int mask = 0;
  if (nargs == 2) {
    mask = env->extract_integer(env, args[1]);
  }

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (rime->api->process_key(rime->session_id, keycode, mask)) {
    return em_t;
  }
  return em_nil;
}

DOCSTRING(get_input, "", "Get rime input.");
static emacs_value get_input(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  const char *input = rime->api->get_input(rime->session_id);

  if (!input) {
    return em_nil;
  } else {
    return env->make_string(env, input, strnlen(input, INPUT_MAXSTRLEN));
  }
}

DOCSTRING(commit_composition, "", "Commit rime composition.");
static emacs_value commit_composition(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (rime->api->commit_composition(rime->session_id)) {
    return em_t;
  }
  return em_nil;
}

DOCSTRING(clear_composition, "", "Clear rime composition.");
static emacs_value clear_composition(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  rime->api->clear_composition(rime->session_id);
  return em_t;
}

DOCSTRING(select_candidate, "NUM", "Select a rime candidate by NUM.");
static emacs_value select_candidate(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  int index = env->extract_integer(env, args[0]);

  if (rime->api->select_candidate_on_current_page(rime->session_id, index)) {
    return em_t;
  }
  return em_nil;
}

// output

DOCSTRING(get_commit, "", "Get rime commit.");
static emacs_value get_commit(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  RIME_STRUCT(RimeCommit, commit);
  if (rime->api->get_commit(rime->session_id, &commit)) {
    if (!commit.text) {
      return em_nil;
    }

    char *commit_str = _copy_string(commit.text);
    rime->api->free_commit(&commit);
    // printf("commit str is %s\n", commit_str);

    return env->make_string(env, commit_str, strlen(commit_str));
  }

  return em_nil;
}

DOCSTRING(get_context, "", "Get rime context.");
static emacs_value get_context(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  RIME_STRUCT(RimeContext, context);
  if (!rime->api->get_context(rime->session_id, &context)) {
    em_signal_rimeerr(env, 2, "Cannot get context.");
    return em_nil;
  }

  size_t result_size = 3;
  emacs_value result_array[result_size];

  // 0. context.commit_text_preview
  char *ctp_str = _copy_string(context.commit_text_preview);
  if (ctp_str)
    result_array[0] = CONS_STRING("commit-text-preview", ctp_str);
  else
    result_array[0] = CONS_NIL("commit-text-preview");

  // 2. context.composition
  size_t composition_size = 5;
  emacs_value composition_array[composition_size];
  composition_array[0] = CONS_INT("length", context.composition.length);
  composition_array[1] = CONS_INT("cursor-pos", context.composition.cursor_pos);
  composition_array[2] = CONS_INT("sel-start", context.composition.sel_start);
  composition_array[3] = CONS_INT("sel-end", context.composition.sel_end);

  char *preedit_str = _copy_string(context.composition.preedit);
  if (preedit_str)
    composition_array[4] = CONS_STRING("preedit", preedit_str);
  else
    // When we don't have a preedit,
    // The composition should be nil.
    return em_nil;
  /* composition_array[4] = CONS_NIL("preedit"); */

  emacs_value composition_value =
      em_list(env, composition_size, composition_array);
  result_array[1] = CONS_VALUE("composition", composition_value);

  // 3. context.menu
  if (context.menu.num_candidates) {
    size_t menu_size = 6;
    emacs_value menu_array[menu_size];
    menu_array[0] = CONS_INT("highlighted-candidate-index",
                             context.menu.highlighted_candidate_index);
    menu_array[1] =
        CONS_VALUE("last-page-p", context.menu.is_last_page ? em_t : em_nil);
    menu_array[2] = CONS_INT("num-candidates", context.menu.num_candidates);
    menu_array[3] = CONS_INT("page-no", context.menu.page_no);
    menu_array[4] = CONS_INT("page-size", context.menu.page_size);
    emacs_value carray[context.menu.num_candidates];
    // Build candidates
    for (int i = 0; i < context.menu.num_candidates; i++) {
      RimeCandidate candidate = context.menu.candidates[i];

      emacs_value value = em_string(env, candidate.text);
      if (candidate.comment) {
        emacs_value comment = em_string(env, candidate.comment);
        value = em_propertize(env, value, ":comment", comment);
      }

      carray[i] = value;
    }

    emacs_value candidates = em_list(env, context.menu.num_candidates, carray);
    menu_array[5] = CONS_VALUE("candidates", candidates);
    emacs_value menu = em_list(env, menu_size, menu_array);
    result_array[2] = CONS_VALUE("menu", menu);
  } else {
    result_array[2] = CONS_NIL("menu");
  }

  // build result
  emacs_value result = em_list(env, result_size, result_array);

  rime->api->free_context(&context);

  return result;
}

DOCSTRING(get_status, "", "Get rime status.");
static emacs_value get_status(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  RIME_STRUCT(RimeStatus, status);
  if (!rime->api->get_status(rime->session_id, &status)) {
    em_signal_rimeerr(env, 2, "Cannot get status.");
    return em_nil;
  }

  size_t result_size = 9;
  emacs_value result_array[result_size];

  char *schema_id = _copy_string(status.schema_id);
  if (schema_id)
    result_array[0] = CONS_STRING("schema_id", schema_id);
  else
    result_array[0] = CONS_NIL("schema_id");

  char *schema_name = _copy_string(status.schema_name);
  if (schema_name)
    result_array[1] = CONS_STRING("schema_name", schema_name);
  else
    result_array[1] = CONS_NIL("schema_name");

  result_array[2] =
      CONS_VALUE("is_disabled", status.is_disabled ? em_t : em_nil);
  result_array[3] =
      CONS_VALUE("is_composing", status.is_composing ? em_t : em_nil);
  result_array[4] =
      CONS_VALUE("is_ascii_mode", status.is_ascii_mode ? em_t : em_nil);
  result_array[5] =
      CONS_VALUE("is_full_shape", status.is_full_shape ? em_t : em_nil);
  result_array[6] =
      CONS_VALUE("is_simplified", status.is_simplified ? em_t : em_nil);
  result_array[7] =
      CONS_VALUE("is_traditional", status.is_traditional ? em_t : em_nil);
  result_array[8] =
      CONS_VALUE("is_ascii_punct", status.is_ascii_punct ? em_t : em_nil);

  // build result
  emacs_value result = em_list(env, result_size, result_array);

  rime->api->free_status(&status);

  return result;
}

DOCSTRING(get_user_config, "USER-CONFIG OPTION &optional RETURN-VALUE-TYPE",
          "Get OPTION of rime USER-CONFIG.\n"
          "The return value type can be set with RETURN-VALUE-TYPE.");
static emacs_value get_user_config(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (nargs < 2) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
    return em_nil;
  }

  const char *config_id = em_get_string(env, args[0]);
  const char *config_key = em_get_string(env, args[1]);
  char *config_type = "cstring";
  if (nargs == 3) {
    config_type = em_get_string(env, args[2]);
  }

  RimeConfig *config = malloc(sizeof(RimeConfig));
  // 注意user_config_open是从user_data_dir下获取
  if (!rime->api->user_config_open(config_id, config)) {
    em_signal_rimeerr(env, 2, "Failed to open user config file.");
    return em_nil;
  }

  bool success = false;
  emacs_value result;
  // printf("get %s for %s\n", config_key, config_type);
  if (strcmp("int", config_type) == 0) {
    int number = 0;
    success = rime->api->config_get_int(config, config_key, &number);
    result = env->make_integer(env, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = 0.0;
    success = rime->api->config_get_double(config, config_key, &number);
    result = env->make_float(env, number);
  } else if (strcmp("bool", config_type) == 0) {
    Bool is_true = false;
    success = rime->api->config_get_bool(config, config_key, &is_true);
    result = is_true ? em_t : em_nil;
  } else {
    const char *string = rime->api->config_get_cstring(config, config_key);
    success = true;
    result = env->make_string(env, string, strnlen(string, CONFIG_MAXSTRLEN));
  }

  rime->api->config_close(config);
  if (!success) {
    em_signal_rimeerr(env, 2, "Failed to get config.");
    return em_nil;
  }

  return result;
}

DOCSTRING(set_user_config, "USER-CONFIG OPTION VALUE &optional VALUE-TYPE",
          "Set rime USER-CONFIG OPTION to VALUE.\n"
          "When VALUE-TYPE is non-nil, VALUE will be converted to this type.");
static emacs_value set_user_config(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
  }

  if (nargs < 3) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
  }

  const char *config_id = em_get_string(env, args[0]);
  const char *config_key = em_get_string(env, args[1]);
  emacs_value value = args[2];
  char *config_type = "string";
  if (nargs == 4) {
    config_type = em_get_string(env, args[3]);
  }

  RimeConfig *config = malloc(sizeof(RimeConfig));
  if (!rime->api->user_config_open(config_id, config)) {
    em_signal_rimeerr(env, 2, "Failed to open user config file.");
    return em_nil;
  }

  if (strcmp("int", config_type) == 0) {
    int number = env->extract_integer(env, value);
    rime->api->config_set_int(config, config_key, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = env->extract_float(env, value);
    rime->api->config_set_double(config, config_key, number);
  } else if (strcmp("bool", config_type) == 0) {
    bool is_true = env->is_not_nil(env, value);
    rime->api->config_set_bool(config, config_key, is_true);
  } else {
    const char *string = em_get_string(env, value);
    rime->api->config_set_string(config, config_key, string);
  }

  rime->api->config_close(config);
  return em_t;
}

DOCSTRING(get_schema_config, "SCHEMA-CONFIG OPTION &optional RETURN-VALUE-TYPE",
          "Get OPTION of rime SCHEMA-CONFIG.\n"
          "The return value type can be set with RETURN-VALUE-TYPE.");
static emacs_value get_schema_config(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (nargs < 2) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
    return em_nil;
  }

  const char *arg0 = em_get_string(env, args[0]);
  const int max_schema_length = 0xff;
  char *schema_id = (char *)malloc(max_schema_length * sizeof(char));
  memset(schema_id, 0, max_schema_length);
  if (arg0 == NULL || strlen(arg0) == 0) {
    if (!rime->api->get_current_schema(rime->session_id, schema_id,
                                       max_schema_length)) {
      em_signal_rimeerr(env, 2, "error get current schema");
      return em_nil;
    }
  } else {
    if (strlen(arg0) > max_schema_length) {
      em_signal_rimeerr(env, 2, "Schema id too long.");
      return em_nil;
    }

    strcpy(schema_id, arg0);
  }

  if (strlen(schema_id) == 0) {
    free(schema_id);
    em_signal_rimeerr(env, 2, "Error length of schema id.");
    return em_nil;
  }

  const char *config_key = em_get_string(env, args[1]);
  char *config_type = "cstring";
  if (nargs == 3) {
    config_type = em_get_string(env, args[2]);
  }

  RimeConfig *config = malloc(sizeof(RimeConfig));
  if (!rime->api->schema_open(schema_id, config)) {
    free(schema_id);
    em_signal_rimeerr(env, 2, "Failed to open schema config file.");
    return em_nil;
  }

  free(schema_id);

  bool success = false;
  emacs_value result;
  // printf("get %s for %s\n", schema_id, config_type);
  if (strcmp("int", config_type) == 0) {
    int number = 0;
    success = rime->api->config_get_int(config, config_key, &number);
    result = env->make_integer(env, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = 0.0;
    success = rime->api->config_get_double(config, config_key, &number);
    result = env->make_float(env, number);
  } else if (strcmp("bool", config_type) == 0) {
    Bool is_true = false;
    success = rime->api->config_get_bool(config, config_key, &is_true);
    result = is_true ? em_t : em_nil;
  } else {
    const char *string = rime->api->config_get_cstring(config, config_key);
    success = true;
    result = env->make_string(env, string, strnlen(string, CONFIG_MAXSTRLEN));
  }

  rime->api->config_close(config);
  if (!success) {
    em_signal_rimeerr(env, 2, "Failed to get config.");
    return em_nil;
  }

  return result;
}

DOCSTRING(set_schema_config, "CONFIG OPTION VALUE &optional VALUE-TYPE",
          "Set rime SCHEMA-CONFIG OPTION to VALUE.\n"
          "When VALUE-TYPE is non-nil, VALUE will be converted to this type.");
static emacs_value set_schema_config(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
  }

  if (nargs < 3) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
  }

  const char *arg0 = em_get_string(env, args[0]);
  const int max_schema_length = 0xff;
  char *schema_id = (char *)malloc(max_schema_length * sizeof(char));
  memset(schema_id, 0, max_schema_length);
  if (arg0 == NULL || strlen(arg0) == 0) {
    if (!rime->api->get_current_schema(rime->session_id, schema_id,
                                       max_schema_length)) {
      em_signal_rimeerr(env, 2, "Error get current schema.");
      return em_nil;
    }
  } else {
    if (strlen(arg0) > max_schema_length) {
      em_signal_rimeerr(env, 2, "Schema id too long.");
      return em_nil;
    }

    strcpy(schema_id, arg0);
  }

  if (strlen(schema_id) == 0) {
    free(schema_id);
    em_signal_rimeerr(env, 2, "Error length of schema id.");
    return em_nil;
  }

  const char *config_key = em_get_string(env, args[1]);
  emacs_value value = args[2];
  char *config_type = "string";
  if (nargs == 4) {
    config_type = em_get_string(env, args[3]);
  }

  RimeConfig *config = (RimeConfig *)malloc(sizeof(RimeConfig));
  if (!rime->api->schema_open(schema_id, config)) {
    free(schema_id);
    em_signal_rimeerr(env, 2, "Failed to open schema config file.");
    return em_nil;
  }

  free(schema_id);
  if (strcmp("int", config_type) == 0) {
    int number = env->extract_integer(env, value);
    rime->api->config_set_int(config, config_key, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = env->extract_float(env, value);
    rime->api->config_set_double(config, config_key, number);
  } else if (strcmp("bool", config_type) == 0) {
    bool is_true = env->is_not_nil(env, value);
    rime->api->config_set_bool(config, config_key, is_true);
  } else {
    const char *string = em_get_string(env, value);
    rime->api->config_set_string(config, config_key, string);
  }

  rime->api->config_close(config);
  return em_t;
}

void liberime_init(emacs_env *env) {
  // Name 'rime' is hardcode in DEFUN micro, so if you edit here,
  // you should edit DEFUN micro too.
  EmacsRime *rime = (EmacsRime *)malloc(sizeof(EmacsRime));

  rime->api = rime_get_api();
  rime->first_run = true; // not used yet

  if (!rime->api) {
    free(rime);
    em_signal_rimeerr(env, 1, "No librime found.");
    return;
  }

  DEFUN("liberime-start", start, 2, 2);
  DEFUN("liberime-search", search, 1, 2);
  DEFUN("liberime-select-schema", select_schema, 1, 1);
  DEFUN("liberime-get-schema-list", get_schema_list, 0, 0);

  // input
  DEFUN("liberime-process-key", process_key, 1, 2);
  DEFUN("liberime-commit-composition", commit_composition, 0, 0);
  DEFUN("liberime-clear-composition", clear_composition, 0, 0);
  DEFUN("liberime-select-candidate", select_candidate, 1, 1);
  DEFUN("liberime-get-input", get_input, 0, 0);

  // output
  DEFUN("liberime-get-commit", get_commit, 0, 0);
  DEFUN("liberime-get-context", get_context, 0, 0);

  // status
  DEFUN("liberime-get-status", get_status, 0, 0);

  // sync
  DEFUN("liberime-get-sync-dir", get_sync_dir, 0, 0);
  DEFUN("liberime-sync-user-data", sync_user_data, 0, 0);
  DEFUN("liberime-finalize", finalize, 0, 0);

  // user config
  DEFUN("liberime-get-user-config", get_user_config, 2, 3);
  DEFUN("liberime-set-user-config", set_user_config, 3, 4);

  // schema config
  // if schema id is nil/empty then get/set current schema config
  DEFUN("liberime-get-schema-config", get_schema_config, 2, 3);
  DEFUN("liberime-set-schema-config", set_schema_config, 3, 4);
}
