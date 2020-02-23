;;; erime.el *- lexical-binding: t; -*-
;;
;; Author: Qiang Fang
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; rime input
;;

;;; Example config with doom emacs:

;; (use-package! erime
;;   :after-call after-find-file pre-command-hook
;;   :defer 1
;;   :init
;;   (setq liberime-user-data-dir
;;         (expand-file-name "local/pyim/rime" doom-private-dir))

;;   (add-hook 'liberime-after-start-hook
;;             (lambda ()
;;               (liberime-select-schema "guhuwubi")))
;;   :config
;;   (setq erime-title "ㄓ"
;;         default-input-method "erime")
;;   )

;;
;;; Code:

(require 'liberime-config)
(require 'posframe nil t)

(defgroup erime nil
  "Rime is a frontend to liberime"
  :group 'leim)

(defcustom erime-probe-list
  '(evil-normal-state-p
    erime-probe-program-mode
    erime-probe-english-context)
  "临时英文模式探针"
  :group 'erime
  :type 'list)

(defcustom erime-prompt-tooltip 'posframe
  " 1. 当这个变量取值为 posframe 时，使用 posframe 包来绘制选词框
    2. 当这个变量取值为 minibuffer 时，使用 minibuffer 做为选词框"
  :group 'erime)

(defface erime-prompt
  '((t (:inherit default :background "#333333" :foreground "#fcdc00")))
  "Face used for the rime page."
  :group 'erime)

(defvar erime-tooltip-posframe-buffer " *erime prompt*")

(defvar erime-local-variable-list
  '(input-method-function
    deactivate-current-input-method-function
    erime-last-punctuation))

(dolist (var erime-local-variable-list)
  (defvar var nil)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defvar erime-menu-keys `(;; Next PageDown
                         ("M-n" . 65366)
                         ;; Prior PageUp
                         ("M-p" . 65365)
                         ;; C-n Down
                         ("C-n" . 65364)
                         ;; Up
                         ("C-p" . 65362)
                         ;; Space
                         ("SPC" . 32)
                         ("RET" . 65293)
                         ("C-m" . 65293)
                         ;; exwm-mode buffer
                         ("<return>" . 65293)
                         ,@(mapcar (lambda (x)
                                     `(,(char-to-string x) . ,x))
                                   (number-sequence ?1 ?9))))

(defvar erime-composition-keys '(("C-d" . 65535)
                                ("<deletechar>" . 65535)
                                ;; Shift+Delete
                                ("C-k" . (65505 65535))
                                ("C-h" . 65288)
                                ;; BackSpace
                                ("DEL" . 65288)
                                ("<backspace>" . 65288)
                                ("<delete>" . 65288)
                                ;; Left
                                ("C-b" . 65361)
                                ;; Right
                                ("C-f" . 65363)
                                ;; Home
                                ("C-a" . 65360)
                                ;; End
                                ("C-e" . 65367)))

(defvar erime-aux-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-h" 'delete-backward-char)
    (define-key map [delete] 'delete-backward-char)
    (define-key map (kbd "DEL") 'delete-backward-char)
    (define-key map [backspace] 'delete-backward-char)
    map))

(defvar erime-map
  (let ((map (make-sparse-keymap)))
    (dolist (i (number-sequence ?\  127))
      (define-key map (char-to-string i) 'erime-translate-key))
    (dolist (i (append erime-menu-keys erime-composition-keys))
      (define-key map (kbd (car i)) 'erime-translate-key))
    map))

(defun erime-probe-program-mode ()
  "中文输入限制在字符串和 comment 中"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((ppss (syntax-ppss (point))))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))

(defun erime-probe-english-context ()
  (or
   ;; 中文后面紧接1个空格切换到英文输入
   ;; \cC represents any character of category “C”, according to “M-x describe-categories”
   (looking-back "\\cc " (max (line-beginning-position) (- (point) 2)))
   ;; 英文,数字后保持英文输入
   (looking-back "[a-zA-Z0-9]" (max (line-beginning-position) (1- (point))))))

(defun erime-english-mode-p ()
  (unless (string-match " *temp*" (buffer-name))
    (cl-some #'(lambda (x)
                 (if (functionp x) (funcall x) nil))
             erime-probe-list)))

;; todo: type v to toggle the previously inserted punctuation
(defun erime-fullwidth-mode-p ()
  (not
   (looking-back "[0-9]\\| " (max (line-beginning-position) (1- (point))))))

(defun erime-activate (name)
  (interactive)
  (setq input-method-function 'erime-input-method
        deactivate-current-input-method-function #'erime-deactivate)
  (when (eq (selected-window)
            (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'erime-exit-from-minibuffer)))

(defun erime-deactivate ()
  (mapc 'kill-local-variable erime-local-variable-list))

(defun erime-exit-from-minibuffer ()
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'erime-exit-from-minibuffer)))

(defun erime-input-method (key)
  (if (or buffer-read-only
          (not enable-multibyte-characters)
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
      (with-silent-modifications
        (unwind-protect
             (let* ((echo-keystrokes 0)
                    (help-char nil)
                    (overriding-terminal-local-map erime-map)
                    ;; bind input-method-function to nil to prevent recursion.
                    (input-method-function nil)
                    (input-method-exit-on-first-char nil)
                    ;; Hide the original `buffer-undo-list'.
                    (buffer-undo-list t)
                    ;; preview string in buffer, see quail.el
                    (input-method-use-echo-area nil)
                    (inhibit-modification-hooks t)
                    (inhibit-quit t)
                    (modified-p (buffer-modified-p))
                    ;; rime local variables
                    (erime-input-buffer
                      (get-buffer-create " *rime-input*"))
                    (erime-translating t)
                    result)
               (with-current-buffer erime-input-buffer
                 (erase-buffer))
               (liberime-clear-composition)
               ;; Push back the last event on the event queue.
               (and key (push key unread-command-events))
               (erime-start-translation)
               (if (stringp result)
                   (mapcar 'identity result)
                   ;; if result are vectors, convert to a list, maybe unecessary
                   (append result nil)))))))

(defun erime-start-translation ()
  (while erime-translating
    (let ((keyseq (read-key-sequence-vector nil nil nil t)))
      (erime-translate-key keyseq))
    ;; continuation check
    ;; liberime-get-commit returns the result only after all being translated
    (if (or result
            (setq result (liberime-get-commit)))
        (progn
          (setq erime-translating nil))
        (let ((context (liberime-get-context)))
          ;; update prompt and continue
          (erime-prompt--refresh))))
  (when (get-buffer erime-tooltip-posframe-buffer)
    (posframe-hide erime-tooltip-posframe-buffer))
  result)

(defun erime-translate-key (keyseq)
  (let* ((keyseq-name (key-description keyseq))
         (key (aref keyseq (1- (length keyseq))))
         keysym-num)
    ;; key processing based on key type
    (cond
      ;; 1. alphabet
      ((and (= (length keyseq-name) 1)
            (string-match "[[:alpha:]]" keyseq-name)
            (not (erime-english-mode-p)))
       (liberime-process-key key)
       (with-current-buffer erime-input-buffer
         (insert key))
       (setq erime-last-punctuation nil))
      ;; 2. same punctuation press twice to go to the next candidate
      ((and (= (length keyseq-name) 1)
            (erime-fullwidth-mode-p)
            (string-match "[[:punct:]]" keyseq-name))
       (liberime-process-key key)
       (if (eq erime-last-punctuation key)
           (progn
             (liberime-select-candidate 1)
             ;; delete the last char
             (push 127 unread-post-input-method-events)
             ;; insert the replacement
             (push (string-to-char (liberime-get-commit)) unread-command-events))
           (liberime-select-candidate 0))
       (setq erime-translating nil)
       (setq erime-last-punctuation key))
      ;; 3. menu key
      ((and (setq keysym-num
                  (cdr (assoc keyseq-name erime-menu-keys))))
       ;; numeric and space char
       (if (and (or (= (length keyseq-name) 1)
                    (string-equal "SPC" keyseq-name))
                (erime-input-empty-p))
           (setq result (char-to-string key))
           (liberime-process-key keysym-num)))
      ;; 4. composition key
      ((and (setq keysym-num
                  (cdr (assoc keyseq-name erime-composition-keys))))
       (liberime-process-key keysym-num)
       (with-current-buffer erime-input-buffer
         (let* ((overriding-terminal-local-map erime-aux-map)
                (bind (key-binding keyseq t)))
           (if bind
               (ignore-errors
                 (call-interactively bind nil keyseq))
               (let (message-log-max)
                 (message "%s is undefined" (key-description keys))
                 (undefined))))))
      (t (setq result (this-command-keys))))))

(defun erime-prompt--format-prompt ()
  (if (not context)
      (with-current-buffer erime-input-buffer
        (concat (buffer-substring-no-properties 1 (point)) "|"
                (buffer-substring-no-properties (point) (point-max))))
      (let* ((composition (alist-get 'composition context))
             (composition-length (alist-get 'length composition))
             (cursor-pos (alist-get 'cursor-pos composition))
             (preedit (alist-get 'preedit composition))
             (menu (alist-get 'menu context))
             (highlighted-candidate-index (alist-get 'highlighted-candidate-index menu))
             (last-page-p (alist-get 'last-page-p menu))
             (num-candidates (alist-get 'num-candidates menu))
             (page-no (alist-get 'page-no menu))
             (candidates (alist-get 'candidates menu))
             (cursor-distance-to-end (- composition-length cursor-pos)))
        (with-current-buffer erime-input-buffer
          (erase-buffer)
          (insert preedit)
          (backward-char cursor-distance-to-end))
        (concat
         (with-current-buffer erime-input-buffer
           (concat (buffer-substring-no-properties 1 (point)) "|"
                   (buffer-substring-no-properties (point) (point-max))))
         ":"
         ;; candidates
         (let (result)
           (dolist (i (number-sequence 0 (1- num-candidates)))
             ;; 高亮当前选择的词条
             (push (if (= i highlighted-candidate-index)
                       (format "[%d.%s]" (1+ i) (nth i candidates))
                       (format "%d.%s" (1+ i) (nth i candidates)))
                   result))
           (concat
            (mapconcat #'identity (reverse result) " ")
            ;; current page number
            (if last-page-p
                (format "(%s<)" (1+ page-no))
                (format "(%s)" (1+ page-no)))))))))

(defun erime-prompt--refresh ()
  (when (and (null unread-command-events)
             (null unread-post-input-method-events))
    (let ((prompt-str (erime-prompt--format-prompt)))
      (if (eq (selected-window)
              (minibuffer-window))
          ;; minibuffer 使用下一行显示候选词
          (erime-prompt--minibuffer-message (concat "\n" prompt-str))
          ;; 普通 buffer
          (if (and (eq erime-prompt-tooltip 'posframe)
                   (not (string-match " *temp*" (buffer-name)))
                   (posframe-valid-p))
              (posframe-show erime-tooltip-posframe-buffer
                             :string prompt-str
                             :position (point)
                          :background-color (face-attribute 'erime-prompt :background)
                          :foreground-color (face-attribute 'erime-prompt :foreground))
              (message (propertize prompt-str 'face 'erime-prompt)))))))

(defun erime-prompt--minibuffer-message (string)
  "minibuffer 中需要将原来显示的信息和选词框整合在一起显示"
  (message nil)
  (let ((inhibit-quit t)
        point-1)
    (save-excursion
      (insert string)
      (setq point-1 (point)))
    (sit-for 1000000)
    (delete-region (point) point-1)
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun posframe-valid-p ()
  "Test posframe's status."
  (and (>= emacs-major-version 26)
       (featurep 'posframe)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(defun erime-input-empty-p ()
  (= (buffer-size erime-input-buffer) 0))

;; add word to ~/.emacs.d/rime/my_phrase.dict.yaml and sort
(defun erime-add-to-user-dict ())

;; add word to ~/.emacs.d/rime/custom_phrase.txt and sort:
;; auto add pinyin code, prompt for confirmation
(defun erime-add-to-custom-phrase ())

;;;###autoload
(defvar erime-title "erime" "The name displayed in mode-line of erime.")

;;;###autoload
(register-input-method "erime" "euc-cn" 'erime-activate erime-title)

(provide 'erime)

;;; erime.el ends here