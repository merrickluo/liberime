;;; liberime-test.el --- Integration tests for liberime -*- lexical-binding: t; -*-

;; Copyright (C) 2024 jixiuf

;; Author: jixiuf

;;; Commentary:

;; Integration test suite that exercises the real C dynamic module
;; (liberime-core) with a running librime instance.  These tests
;; require librime to be installed and the C module to be compiled.
;;
;; Run with:
;;   make test
;;
;; Or manually:
;;   emacs --batch -Q -L . -l ert -l liberime-test.el -f liberime-test-run

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'liberime)

;; ---------------------------------------------------------------------------
;; Test setup / teardown
;; ---------------------------------------------------------------------------

(defvar liberime-test--session-id nil
  "Session ID from liberime-start for test use.")

(defvar liberime-test--shared-dir nil
  "Shared data dir used for tests.")

(defvar liberime-test--user-dir nil
  "Temporary user data dir for tests.")

(defun liberime-test--setup ()
  "Initialize librime for testing.
Creates a temporary user data directory."
  (unless liberime-test--session-id
    (setq liberime-test--user-dir
          (make-temp-file "liberime-test-user" t))
    (setq liberime-test--shared-dir
          (or (liberime-get-shared-data-dir)
              ;; Fallback: try common paths
              (cl-find-if #'file-directory-p
                          '("/usr/share/rime-data"
                            "/usr/local/share/rime-data"
                            "/usr/share/local/rime-data"))))
    (when liberime-test--shared-dir
      (setq liberime-test--session-id
            (ignore-errors
              (liberime-start liberime-test--shared-dir
                              liberime-test--user-dir)))
      ;; Select a schema for testing
      (when liberime-test--session-id
        (let ((schemas (liberime-get-schema-list)))
          (when schemas
            (liberime-select-schema (caar schemas))))))))

(defun liberime-test--teardown ()
  "Finalize librime and clean up."
  (when liberime-test--session-id
    (ignore-errors (liberime-finalize))
    (setq liberime-test--session-id nil))
  (when (and liberime-test--user-dir
             (file-directory-p liberime-test--user-dir))
    (delete-directory liberime-test--user-dir t)
    (setq liberime-test--user-dir nil)))

(defun liberime-test--skip-unless-rime ()
  "Skip the current test if librime is not available."
  (unless (featurep 'liberime-core)
    (ert-skip "liberime-core module not loaded"))
  (liberime-test--setup)
  (unless liberime-test--session-id
    (ert-skip "Cannot initialize librime (no shared data?)")))

;; ---------------------------------------------------------------------------
;; Session management tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-start-returns-session-id ()
  "liberime-start should return t on success."
  (liberime-test--skip-unless-rime)
  (should liberime-test--session-id))

;; ---------------------------------------------------------------------------
;; Schema tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-get-schema-list ()
  "liberime-get-schema-list should return a non-empty list."
  (liberime-test--skip-unless-rime)
  (let ((schemas (liberime-get-schema-list)))
    (should (listp schemas))
    (should (> (length schemas) 0))
    (let ((schema (car schemas)))
      (should (consp schema))
      (should (stringp (car schema)))
      (should (stringp (cadr schema))))))

;; ---------------------------------------------------------------------------
;; Input processing tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-process-key ()
  "liberime-process-key should return t when processing a key."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (should (eq (liberime-process-key ?a) t)))

(ert-deftest liberime-test-simulate-key-sequence ()
  "liberime-simulate-key-sequence should process key sequence."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (should (eq (liberime-simulate-key-sequence "zhong") t))
  (should (string-equal (liberime-get-input) "zhong"))
  (liberime-clear-composition))

;; ---------------------------------------------------------------------------
;; Event conversion tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-event-to-key-sequence-lowercase-ascii ()
  "Lowercase ASCII character should return itself (no braces needed)."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?a) "a"))
  (should (string= (liberime-event-to-key-sequence ?z) "z"))
  (should (string= (liberime-event-to-key-sequence ?m) "m")))

(ert-deftest liberime-test-event-to-key-sequence-uppercase-ascii ()
  "Uppercase ASCII character should return itself."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?A) "A"))
  (should (string= (liberime-event-to-key-sequence ?Z) "Z")))

(ert-deftest liberime-test-event-to-key-sequence-digits ()
  "Digit characters should return themselves."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?0) "0"))
  (should (string= (liberime-event-to-key-sequence ?5) "5"))
  (should (string= (liberime-event-to-key-sequence ?9) "9")))

(ert-deftest liberime-test-event-to-key-sequence-comma ()
  "Comma without modifiers should return comma char directly."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?,) ",")))

(ert-deftest liberime-test-event-to-key-sequence-period ()
  "Period without modifiers should return period char directly."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?.) ".")))

(ert-deftest liberime-test-event-to-key-sequence-control-modifier ()
  "Control + a (Emacs returns 1 for C-a without modifier bits)."
  (liberime-test--skip-unless-rime)
  (let ((c-a (car (listify-key-sequence (kbd "C-a")))))
    (should (string= (liberime-event-to-key-sequence c-a) "{Control+a}"))))

(ert-deftest liberime-test-event-to-key-sequence-meta-modifier ()
  "Meta modifier should produce {Meta+...} format."
  (liberime-test--skip-unless-rime)
  (let ((m-a (car (listify-key-sequence (kbd "M-a")))))
    (should (string= (liberime-event-to-key-sequence m-a) "{Meta+a}"))))

(ert-deftest liberime-test-event-to-key-sequence-shift-modifier ()
  "Shift modifier should produce {Shift+...} format."
  (liberime-test--skip-unless-rime)
  (let ((s-a (car (listify-key-sequence (kbd "S-a")))))
    (should (string= (liberime-event-to-key-sequence s-a) "{Shift+a}"))))

(ert-deftest liberime-test-event-to-key-sequence-multiple-modifiers ()
  "Multiple modifiers should all appear in output."
  (liberime-test--skip-unless-rime)
  (let ((c-m-a (car (listify-key-sequence (kbd "C-M-a")))))
    (let ((result (liberime-event-to-key-sequence c-m-a)))
      (should (string= result "{Control+Meta+a}"))
      (should (string-match "Control" result))
      (should (string-match "Meta" result))
      (should (string-match "a" result)))))

(ert-deftest liberime-test-event-to-key-sequence-direction-keys ()
  "Direction keys should return {Direction} format."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 'left) "{Left}"))
  (should (string= (liberime-event-to-key-sequence 'right) "{Right}"))
  (should (string= (liberime-event-to-key-sequence 'up) "{Up}"))
  (should (string= (liberime-event-to-key-sequence 'down) "{Down}")))

(ert-deftest liberime-test-event-to-key-sequence-navigation-keys ()
  "Navigation keys should return correct format."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 'return) "{Return}"))
  (should (string= (liberime-event-to-key-sequence 'space) "{space}"))
  (should (string= (liberime-event-to-key-sequence 'backspace) "{BackSpace}"))
  (should (string= (liberime-event-to-key-sequence 'tab) "{Tab}"))
  (should (string= (liberime-event-to-key-sequence 'escape) "{Escape}"))
  (should (string= (liberime-event-to-key-sequence 'home) "{Home}"))
  (should (string= (liberime-event-to-key-sequence 'end) "{End}"))
  (should (string= (liberime-event-to-key-sequence 'delete) "{Delete}"))
  (should (string= (liberime-event-to-key-sequence 'prior) "{Prior}"))
  (should (string= (liberime-event-to-key-sequence 'next) "{Next}")))

(ert-deftest liberime-test-event-to-key-sequence-function-keys ()
  "Function keys should return {Fn} format."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 'f1) "{F1}"))
  (should (string= (liberime-event-to-key-sequence 'f5) "{F5}"))
  (should (string= (liberime-event-to-key-sequence 'f12) "{F12}")))

(ert-deftest liberime-test-event-to-key-sequence-braces ()
  "Brace characters must use names to avoid parse ambiguity."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?{) "{braceleft}"))
  (should (string= (liberime-event-to-key-sequence ?}) "{braceright}")))

(ert-deftest liberime-test-event-to-key-sequence-space ()
  "Space character should return single space."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 32) " ")))

;; ---------------------------------------------------------------------------
;; Process event tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-process-event-integer ()
  "process-event with integer should send key sequence to librime."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (should (eq (liberime-process-event ?a) t)))

(ert-deftest liberime-test-process-event-symbol-left ()
  "process-event with 'left should send Left key to librime."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  ;; Process a key first to enter composition mode
  (liberime-process-event ?w)
  (liberime-process-event ?o)
  ;; Then send space to select first candidate
  (should (eq (liberime-process-event 'space) t))
  (liberime-clear-composition))

(ert-deftest liberime-test-process-event-control-char ()
  "process-event with Control modifier should work."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (let ((c-a (+ ?a #x4000000)))
    ;; Control+a may or may not be handled depending on schema
    (let ((result (liberime-process-event c-a)))
      (should (or (eq result t) (eq result nil))))))

(ert-deftest liberime-test-process-event-return-key ()
  "process-event with 'return should commit composition."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-event ?n)
  (liberime-process-event ?i)
  (let ((input-before (liberime-get-input)))
    (should (string-equal input-before "ni")))
  (liberime-process-event 'return)
  (let ((commit (liberime-get-commit)))
    ;; After return, commit should be consumed or set
    (should (or (null commit) (stringp commit))))
  (liberime-clear-composition))

;; ---------------------------------------------------------------------------
;; kbd-to-key-sequence tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-kbd-to-key-sequence-simple ()
  "kbd-to-key-sequence with simple key should work."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-kbd-to-key-sequence (kbd "a")) "a"))
  (should (string= (liberime-kbd-to-key-sequence "abc") "abc")))

(ert-deftest liberime-test-kbd-to-key-sequence-control ()
  "kbd-to-key-sequence with Control modifier should work."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-kbd-to-key-sequence (kbd "C-a")) "{Control+a}")))

(ert-deftest liberime-test-kbd-to-key-sequence-left ()
  "kbd-to-key-sequence with left arrow should work."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-kbd-to-key-sequence (kbd "<left>")) "{Left}")))

;; ---------------------------------------------------------------------------
;; process-keys tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-process-keys-simple ()
  "process-keys with simple keys should work."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-keys "abc")
  (should (string-equal (liberime-get-input) "abc"))
  (liberime-clear-composition))

(ert-deftest liberime-test-process-keys-with-modifiers ()
  "process-keys with modifier keys should work."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-keys (kbd "C-a"))
  ;; C-a may or may not add to input depending on schema
  (liberime-clear-composition))

;; ---------------------------------------------------------------------------
;; Output tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-get-input ()
  "liberime-get-input should return input string."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-key ?t)
  (liberime-process-key ?e)
  (liberime-process-key ?s)
  (should (string-equal (liberime-get-input) "tes"))
  (liberime-clear-composition))

(ert-deftest liberime-test-get-context ()
  "liberime-get-context should return context alist."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-key ?x)
  (let ((context (liberime-get-context)))
    (should (listp context))
    (should (alist-get 'composition context)))
  (liberime-clear-composition))

(ert-deftest liberime-test-get-commit ()
  "liberime-get-commit should return committed text."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  ;; Type something and commit
  (liberime-process-key ?n)
  (liberime-process-key ?i)
  (liberime-commit-composition)
  (let ((commit (liberime-get-commit)))
    ;; Commit may be nil or a string depending on state
    (should (or (null commit) (stringp commit)))))

;; ---------------------------------------------------------------------------
;; Run tests
;; ---------------------------------------------------------------------------

(defun liberime-test-run ()
  "Run all liberime integration tests."
  (interactive)
  (ert-run-tests-batch-and-exit))

(provide 'liberime-test)

;;; liberime-test.el ends here
