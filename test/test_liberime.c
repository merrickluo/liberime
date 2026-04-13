/*
 * test_liberime.c -- Minimal C unit tests for liberime internal helpers.
 *
 * These tests exercise internal C utility functions WITHOUT requiring
 * an Emacs runtime or a running librime instance.  They test pure C
 * logic such as string copying and linked-list management.
 *
 * Build & run:
 *   make test-c
 *
 * Or manually:
 *   gcc -O2 -Wall -I emacs-module/$(emacs --batch --eval '(princ
 * emacs-major-version)') \ -o test/test_liberime test/test_liberime.c -lrime
 *   ./test/test_liberime
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Include key_table functions for testing */
#include "../src/key_table.h"

/* Link key_table implementation */
#include "../src/key_table.c"

/* ---------------------------------------------------------------------------
 * Re-implement the internal helpers under test (they are static in
 * liberime-core.c, so we copy them here for isolated testing).
 * ---------------------------------------------------------------------------*/

#define CANDIDATE_MAXSTRLEN 1024

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

typedef struct _CandidateLinkedList {
  char *text;
  char *comment;
  struct _CandidateLinkedList *next;
} CandidateLinkedList;

void free_candidate_list(CandidateLinkedList *list) {
  CandidateLinkedList *next = list;
  while (next) {
    CandidateLinkedList *temp = next;
    next = temp->next;
    if (temp->text)
      free(temp->text);
    if (temp->comment)
      free(temp->comment);
    free(temp);
  }
}

/* ---------------------------------------------------------------------------
 * Test infrastructure
 * ---------------------------------------------------------------------------*/

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name)                                                             \
  static void test_##name(void);                                               \
  static void run_test_##name(void) {                                          \
    tests_run++;                                                               \
    printf("  TEST %-50s ", #name);                                            \
    test_##name();                                                             \
    tests_passed++;                                                            \
    printf("PASS\n");                                                          \
  }                                                                            \
  static void test_##name(void)

#define ASSERT(cond)                                                           \
  do {                                                                         \
    if (!(cond)) {                                                             \
      printf("FAIL\n    assertion failed: %s\n    at %s:%d\n", #cond,          \
             __FILE__, __LINE__);                                              \
      tests_failed++;                                                          \
      tests_passed--; /* undo the pre-increment in run_ */                     \
      return;                                                                  \
    }                                                                          \
  } while (0)

#define ASSERT_STR_EQ(a, b)                                                    \
  do {                                                                         \
    if (strcmp((a), (b)) != 0) {                                               \
      printf("FAIL\n    expected \"%s\" == \"%s\"\n    at %s:%d\n", (a), (b),  \
             __FILE__, __LINE__);                                              \
      tests_failed++;                                                          \
      tests_passed--;                                                          \
      return;                                                                  \
    }                                                                          \
  } while (0)

/* ---------------------------------------------------------------------------
 * _copy_string tests
 * ---------------------------------------------------------------------------*/

TEST(copy_string_basic) {
  char *result = _copy_string("hello");
  ASSERT(result != NULL);
  ASSERT_STR_EQ(result, "hello");
  free(result);
}

TEST(copy_string_empty) {
  char *result = _copy_string("");
  ASSERT(result != NULL);
  ASSERT_STR_EQ(result, "");
  free(result);
}

TEST(copy_string_null) {
  char *result = _copy_string(NULL);
  ASSERT(result == NULL);
}

TEST(copy_string_chinese) {
  /* UTF-8 encoded Chinese characters */
  char *result = _copy_string("你好世界");
  ASSERT(result != NULL);
  ASSERT_STR_EQ(result, "你好世界");
  free(result);
}

TEST(copy_string_long) {
  /* Test string near max length */
  char buf[CANDIDATE_MAXSTRLEN + 10];
  memset(buf, 'a', sizeof(buf) - 1);
  buf[sizeof(buf) - 1] = '\0';
  char *result = _copy_string(buf);
  ASSERT(result != NULL);
  /* Should be truncated to CANDIDATE_MAXSTRLEN */
  ASSERT(strlen(result) == CANDIDATE_MAXSTRLEN);
  free(result);
}

TEST(copy_string_is_deep_copy) {
  char original[] = "hello";
  char *result = _copy_string(original);
  ASSERT(result != original); /* different pointer */
  ASSERT_STR_EQ(result, "hello");
  /* Modifying original should not affect copy */
  original[0] = 'X';
  ASSERT_STR_EQ(result, "hello");
  free(result);
}

/* ---------------------------------------------------------------------------
 * CandidateLinkedList tests
 * ---------------------------------------------------------------------------*/

TEST(free_candidate_list_empty) {
  /* Single node with no text */
  CandidateLinkedList *list = malloc(sizeof(CandidateLinkedList));
  list->text = NULL;
  list->comment = NULL;
  list->next = NULL;
  free_candidate_list(list); /* should not crash */
}

TEST(free_candidate_list_single) {
  CandidateLinkedList *list = malloc(sizeof(CandidateLinkedList));
  list->text = _copy_string("hello");
  list->comment = _copy_string("comment");
  list->next = NULL;
  free_candidate_list(list);
}

TEST(free_candidate_list_multiple) {
  CandidateLinkedList *n1 = malloc(sizeof(CandidateLinkedList));
  CandidateLinkedList *n2 = malloc(sizeof(CandidateLinkedList));
  CandidateLinkedList *n3 = malloc(sizeof(CandidateLinkedList));
  n1->text = _copy_string("first");
  n1->comment = NULL;
  n1->next = n2;
  n2->text = _copy_string("second");
  n2->comment = _copy_string("note");
  n2->next = n3;
  n3->text = _copy_string("third");
  n3->comment = NULL;
  n3->next = NULL;
  free_candidate_list(n1);
}

/* ---------------------------------------------------------------------------
 * Emacs event to key sequence tests
 * ---------------------------------------------------------------------------*/

TEST(emacs_event_keycode_basic) {
  ASSERT(emacs_event_keycode('a') == 'a');
  ASSERT(emacs_event_keycode(32) == 32);
  ASSERT(emacs_event_keycode(65) == 65);
}

TEST(emacs_event_has_modifier) {
  ASSERT(emacs_event_has_modifier('a') == 0);
  ASSERT(emacs_event_has_modifier('a' + EMACS_CHAR_CTL) != 0);
  ASSERT(emacs_event_has_modifier('a' + EMACS_CHAR_META) != 0);
  ASSERT(emacs_event_has_modifier('a' + EMACS_CHAR_SHIFT) != 0);
}

TEST(emacs_int_event_lowercase) {
  char buf[256];
  ASSERT(emacs_int_event_to_key_sequence('a', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "a");
}

TEST(emacs_int_event_uppercase) {
  char buf[256];
  ASSERT(emacs_int_event_to_key_sequence('A', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "A");
}

TEST(emacs_int_event_control) {
  char buf[256];
  /* Emacs (read-event) returns 1 for C-a (ASCII 0x01), not ('a' + EMACS_CHAR_CTL) */
  int c_a = 1;
  ASSERT(emacs_int_event_to_key_sequence(c_a, buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+a}");
}

TEST(emacs_int_event_meta) {
  char buf[256];
  int m_a = 'a' + EMACS_CHAR_META;
  ASSERT(emacs_int_event_to_key_sequence(m_a, buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Meta+a}");
}

TEST(emacs_int_event_shift) {
  char buf[256];
  int s_a = 'a' + EMACS_CHAR_SHIFT;
  ASSERT(emacs_int_event_to_key_sequence(s_a, buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Shift+a}");
}

TEST(emacs_int_event_control_meta) {
  char buf[256];
  /* C-M-a: keycode is 1 (C-a) with Meta modifier */
  int c_m_a = 1 + EMACS_CHAR_META;
  ASSERT(emacs_int_event_to_key_sequence(c_m_a, buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+Meta+a}");
}

TEST(emacs_int_event_multiple_modifiers) {
  char buf[256];
  int c_m_s_a = 1 + EMACS_CHAR_CTL + EMACS_CHAR_META + EMACS_CHAR_SHIFT;
  ASSERT(emacs_int_event_to_key_sequence(c_m_s_a, buf, sizeof(buf)) == 0);
  ASSERT(strstr(buf, "Control") != NULL);
  ASSERT(strstr(buf, "Shift") != NULL);
  ASSERT(strstr(buf, "Meta") != NULL);
  ASSERT(strstr(buf, "a") != NULL);
}

TEST(emacs_int_event_punctuation) {
  char buf[256];
  /* Punctuation chars without modifiers output directly */
  ASSERT(emacs_int_event_to_key_sequence(',', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, ",");
  ASSERT(emacs_int_event_to_key_sequence('.', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, ".");
  ASSERT(emacs_int_event_to_key_sequence('!', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "!");
  ASSERT(emacs_int_event_to_key_sequence('[', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "[");
  ASSERT(emacs_int_event_to_key_sequence(';', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, ";");
}

TEST(emacs_int_event_comma_with_control) {
  char buf[256];
  /* C-, has Control modifier, uses braces */
  int c_comma = ',' + EMACS_CHAR_CTL;
  ASSERT(emacs_int_event_to_key_sequence(c_comma, buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+comma}");
}

TEST(emacs_int_event_space) {
  char buf[256];
  ASSERT(emacs_int_event_to_key_sequence(' ', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, " ");
}

TEST(emacs_int_event_braces) {
  char buf[256];
  /* Braces must always use names to avoid KeySequence::Parse ambiguity */
  ASSERT(emacs_int_event_to_key_sequence('{', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{braceleft}");
  ASSERT(emacs_int_event_to_key_sequence('}', buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{braceright}");
  int c_lbrace = '{' + EMACS_CHAR_CTL;
  ASSERT(emacs_int_event_to_key_sequence(c_lbrace, buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+braceleft}");
}

TEST(emacs_int_event_modifiers_with_named_key) {
  char buf[256];
  /* Test modifier + named key combinations */
  int c_left = 0xff51 + EMACS_CHAR_CTL;  /* XK_Left + Control */
  ASSERT(emacs_int_event_to_key_sequence(c_left, buf, sizeof(buf)) == 0);
  ASSERT(strstr(buf, "Control") != NULL);
  ASSERT(strstr(buf, "Left") != NULL);

  int m_f1 = 0xffbe + EMACS_CHAR_META;  /* XK_F1 + Meta */
  ASSERT(emacs_int_event_to_key_sequence(m_f1, buf, sizeof(buf)) == 0);
  ASSERT(strstr(buf, "Meta") != NULL);
  ASSERT(strstr(buf, "F1") != NULL);

  int c_semi = ';' + EMACS_CHAR_CTL;
  ASSERT(emacs_int_event_to_key_sequence(c_semi, buf, sizeof(buf)) == 0);
  ASSERT(strstr(buf, "Control") != NULL);
  ASSERT(strstr(buf, "semicolon") != NULL);
}

TEST(emacs_symbol_lowercase) {
  char buf[256];
  ASSERT(emacs_symbol_to_key_sequence("left", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Left}");
}

TEST(emacs_symbol_capitalized) {
  char buf[256];
  ASSERT(emacs_symbol_to_key_sequence("Return", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Return}");
}

TEST(emacs_symbol_with_modifiers) {
  char buf[256];
  /* Test modifier prefixes in symbol names */
  ASSERT(emacs_symbol_to_key_sequence("C-left", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+Left}");

  ASSERT(emacs_symbol_to_key_sequence("M-f1", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Meta+F1}");

  ASSERT(emacs_symbol_to_key_sequence("C-M-left", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+Meta+Left}");

  ASSERT(emacs_symbol_to_key_sequence("C-return", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Control+Return}");

  ASSERT(emacs_symbol_to_key_sequence("s-a", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{Super+a}");
}

TEST(emacs_symbol_space) {
  char buf[256];
  ASSERT(emacs_symbol_to_key_sequence("space", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{space}");
}

TEST(emacs_symbol_comma) {
  /* "comma" is not a symbol Emacs returns; this tests the name lookup */
  char buf[256];
  ASSERT(emacs_symbol_to_key_sequence("comma", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{comma}");
}

TEST(emacs_symbol_unknown) {
  char buf[256];
  ASSERT(emacs_symbol_to_key_sequence("unknown_key_xyz", buf, sizeof(buf)) == 0);
  ASSERT_STR_EQ(buf, "{unknown_key_xyz}");
}

TEST(emacs_symbol_null_input) {
  char buf[256];
  ASSERT(emacs_symbol_to_key_sequence(NULL, buf, sizeof(buf)) == -1);
  ASSERT(emacs_symbol_to_key_sequence("left", NULL, sizeof(buf)) == -1);
  ASSERT(emacs_symbol_to_key_sequence("left", buf, 0) == -1);
}

/* ---------------------------------------------------------------------------
 * Main
 * ---------------------------------------------------------------------------*/

int main(void) {
  printf("Running liberime C unit tests...\n\n");

  /* _copy_string tests */
  run_test_copy_string_basic();
  run_test_copy_string_empty();
  run_test_copy_string_null();
  run_test_copy_string_chinese();
  run_test_copy_string_long();
  run_test_copy_string_is_deep_copy();

  /* linked list tests */
  run_test_free_candidate_list_empty();
  run_test_free_candidate_list_single();
  run_test_free_candidate_list_multiple();

  /* emacs event to key sequence tests */
  run_test_emacs_event_keycode_basic();
  run_test_emacs_event_has_modifier();
  run_test_emacs_int_event_lowercase();
  run_test_emacs_int_event_uppercase();
  run_test_emacs_int_event_control();
  run_test_emacs_int_event_meta();
  run_test_emacs_int_event_shift();
  run_test_emacs_int_event_control_meta();
  run_test_emacs_int_event_multiple_modifiers();
  run_test_emacs_int_event_punctuation();
  run_test_emacs_int_event_comma_with_control();
  run_test_emacs_int_event_space();
  run_test_emacs_int_event_modifiers_with_named_key();
  run_test_emacs_symbol_lowercase();
  run_test_emacs_symbol_capitalized();
  run_test_emacs_symbol_with_modifiers();
  run_test_emacs_symbol_space();
  run_test_emacs_symbol_comma();
  run_test_emacs_symbol_unknown();
  run_test_emacs_symbol_null_input();

  printf("\n%d tests run, %d passed, %d failed.\n", tests_run, tests_passed,
         tests_failed);

  return tests_failed > 0 ? 1 : 0;
}
