// encoding: utf-8
//
#ifndef LIBERIME_KEY_TABLE_H_
#define LIBERIME_KEY_TABLE_H_

#include <stddef.h>

/* XK_VoidSymbol: defined here to avoid requiring rime_api.h or X11 headers
 * in standalone builds (e.g. test_librimel.c). */
#ifndef XK_VoidSymbol
#define XK_VoidSymbol 0xffffff
#endif

// 给定modifier文字，返回mask值
// 例如 rime_get_modifier_by_name("Alt") == (1 << 3)
// 如果不认得所给的键名，返回 0
int rime_get_modifier_by_name(const char *name);

// 给一个数值，取得最低的非0位所对应的modifier文字
// 例如 rime_get_modifier_name(12) == "Control"
// 取不到则返回 NULL
const char *rime_get_modifier_name(int modifier);

// 由键名取得键值
// 查无此键则返回 XK_VoidSymbol
int rime_get_keycode_by_name(const char *name);

// 由键值取得键名
// 不认得此键，则返回 NULL
const char *rime_get_key_name(int keycode);

/* ========================================================================
 * Emacs event to key sequence conversion
 * ======================================================================== */

/* Emacs modifier bit definitions */
#define EMACS_CHAR_SHIFT 0x2000000
#define EMACS_CHAR_CTL 0x4000000
#define EMACS_CHAR_ALT 0x0400000
#define EMACS_CHAR_SUPER 0x0800000
#define EMACS_CHAR_HYPER 0x1000000
#define EMACS_CHAR_META 0x8000000

/* Extract the raw keycode from Emacs event (lower 21 bits). */
int emacs_event_keycode(int event);

/* Check if an Emacs event has any modifier bits set. */
int emacs_event_has_modifier(int event);

/* Convert an Emacs integer event (with modifiers) to a librime key sequence.
   EVENT is the raw integer from read-event.
   BUF is the output buffer, BUFSZ is its size.
   Returns 0 on success, -1 if the event is not a valid character event. */
int emacs_int_event_to_key_sequence(int event, char *buf, size_t bufsz);

/* Convert a symbol name (like "left", "return", "f1") to a librime key
   sequence. SYM_NAME is the symbol name string. BUF is the output buffer, BUFSZ
   is its size. Returns 0 on success, -1 if the symbol is not recognized. */
int emacs_symbol_to_key_sequence(const char *sym_name, char *buf, size_t bufsz);

#endif // LIBERIME_KEY_TABLE_H_
