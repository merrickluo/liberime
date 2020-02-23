;;; liberime-config.el --- setup liberime automatically

;; Author: Qiang Fang
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; load liberime by default

;;; Code:

(require 'liberime-config)
(require 'posframe nil t)

(defgroup rime nil
  "Rime is a frontend to liberime"
  :group 'leim)

(defcustom rime-probe-list
  '(evil-normal-state-p
    rime-probe-program-mode
    rime-probe-english-context)
  "临时英文模式探针"
  :group 'rime
  :type 'list)

(defcustom rime-prompt-tooltip 'posframe
  " 1. 当这个变量取值为 posframe 时，使用 posframe 包来绘制选词框
    2. 当这个变量取值为 minibuffer 时，使用 minibuffer 做为选词框"
  :group 'rime)

(defface rime-prompt
  '((t (:inherit default :background "#333333" :foreground "#fcdc00")))
  "Face used for the rime page."
  :group 'rime)

(defvar rime-tooltip-posframe-buffer " *rime prompt*")

(defvar rime-local-variable-list
  '(input-method-function
    deactivate-current-input-method-function
    rime-last-punctuation))

(dolist (var rime-local-variable-list)
  (defvar var nil)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defvar rime-menu-keys `(;; Next PageDown
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

(defvar rime-composition-keys '(("C-d" . 65535)
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

(defvar rime-aux-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-h" 'delete-backward-char)
    (define-key map [delete] 'delete-backward-char)
    (define-key map (kbd "DEL") 'delete-backward-char)
    (define-key map [backspace] 'delete-backward-char)
    map))

(defvar rime-map
  (let ((map (make-sparse-keymap)))
    (dolist (i (number-sequence ?\  127))
      (define-key map (char-to-string i) 'rime-translate-key))
    (dolist (i (append rime-menu-keys rime-composition-keys))
      (define-key map (kbd (car i)) 'rime-translate-key))
    map))

(defun rime-probe-program-mode ()
  "中文输入限制在字符串和 comment 中"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((ppss (syntax-ppss (point))))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))

(defun rime-probe-english-context ()
  (or
   ;; 中文后面紧接1个空格切换到英文输入
   ;; \cC represents any character of category “C”, according to “M-x describe-categories”
   (looking-back "\\cc " (max (line-beginning-position) (- (point) 2)))
   ;; 英文,数字后保持英文输入
   (looking-back "[a-zA-Z0-9]" (max (line-beginning-position) (1- (point))))))

(defun rime-english-mode-p ()
  (unless (string-match " *temp*" (buffer-name))
    (cl-some #'(lambda (x)
                 (if (functionp x) (funcall x) nil))
             rime-probe-list)))

;; todo: type v to toggle the previously inserted punctuation 
(defun rime-fullwidth-mode-p ()
  (not
   (looking-back "[0-9]\\| " (max (line-beginning-position) (1- (point))))))

(defun rime-activate (name)
  (interactive)
  (setq input-method-function 'rime-input-method
        deactivate-current-input-method-function #'rime-deactivate)
  (when (eq (selected-window)
            (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'rime-exit-from-minibuffer)))

(defun rime-deactivate ()
  (mapc 'kill-local-variable rime-local-variable-list))

(defun rime-exit-from-minibuffer ()
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'rime-exit-from-minibuffer)))

(defun rime-input-method (key)
  (if (or buffer-read-only
          (not enable-multibyte-characters)
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
      (with-silent-modifications
        (unwind-protect
             (let* ((echo-keystrokes 0)
                    (help-char nil)
                    (overriding-terminal-local-map rime-map)
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
                    (rime-input-buffer
                      (get-buffer-create " *rime-input*"))
                    (rime-translating t)
                    result)
               (with-current-buffer rime-input-buffer
                 (erase-buffer))
               (liberime-clear-composition)
               ;; Push back the last event on the event queue.
               (and key (push key unread-command-events))
               (rime-start-translation)
               (if (stringp result)
                   (mapcar 'identity result)
                   ;; if result are vectors, convert to a list, maybe unecessary
                   (append result nil)))))))

(defun rime-start-translation ()
  (while rime-translating
    (let ((keyseq (read-key-sequence-vector nil nil nil t)))
      (rime-translate-key keyseq))
    ;; continuation check
    ;; liberime-get-commit returns the result only after all being translated
    (if (or result
            (setq result (liberime-get-commit)))
        (progn
          (setq rime-translating nil))
        (let ((context (liberime-get-context)))
          ;; update prompt and continue
          (rime-prompt--refresh))))
  (when (get-buffer rime-tooltip-posframe-buffer)
    (posframe-hide rime-tooltip-posframe-buffer))
  result)

(defun rime-translate-key (keyseq)
  (let* ((keyseq-name (key-description keyseq))
         (key (aref keyseq (1- (length keyseq))))
         keysym-num)
    ;; key processing based on key type
    (cond
      ;; 1. alphabet
      ((and (= (length keyseq-name) 1)
            (string-match "[[:alpha:]]" keyseq-name)
            (not (rime-english-mode-p)))
       (liberime-process-key key)
       (with-current-buffer rime-input-buffer
         (insert key))
       (setq rime-last-punctuation nil))
      ;; 2. same punctuation press twice to go to the next candidate
      ((and (= (length keyseq-name) 1)
            (rime-fullwidth-mode-p)
            (string-match "[[:punct:]]" keyseq-name))
       (liberime-process-key key)
       (if (eq rime-last-punctuation key)
           (progn
             (liberime-select-candidate 1)
             ;; delete the last char
             (push 127 unread-post-input-method-events)
             ;; insert the replacement
             (push (string-to-char (liberime-get-commit)) unread-command-events))
           (liberime-select-candidate 0))
       (setq rime-translating nil)
       (setq rime-last-punctuation key))
      ;; 3. menu key
      ((and (setq keysym-num
                  (cdr (assoc keyseq-name rime-menu-keys))))
       ;; numeric and space char
       (if (and (or (= (length keyseq-name) 1)
                    (string-equal "SPC" keyseq-name))
                (rime-input-empty-p))
           (setq result (char-to-string key))
           (liberime-process-key keysym-num)))
      ;; 4. composition key
      ((and (setq keysym-num
                  (cdr (assoc keyseq-name rime-composition-keys))))
       (liberime-process-key keysym-num)
       (with-current-buffer rime-input-buffer
         (let* ((overriding-terminal-local-map rime-aux-map)
                (bind (key-binding keyseq t)))
           (if bind
               (ignore-errors
                 (call-interactively bind nil keyseq))
               (let (message-log-max)
                 (message "%s is undefined" (key-description keys))
                 (undefined))))))
      (t (setq result (this-command-keys))))))

(defun rime-prompt--format-prompt ()
  (if (not context)
      (with-current-buffer rime-input-buffer
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
        (with-current-buffer rime-input-buffer
          (erase-buffer)
          (insert preedit)
          (backward-char cursor-distance-to-end))
        (concat
         (with-current-buffer rime-input-buffer
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

(defun rime-prompt--refresh ()
  (when (and (null unread-command-events)
             (null unread-post-input-method-events))
    (let ((prompt-str (rime-prompt--format-prompt)))
      (if (eq (selected-window)
              (minibuffer-window))
          ;; minibuffer 使用下一行显示候选词
          (rime-prompt--minibuffer-message (concat "\n" prompt-str))
          ;; 普通 buffer 
          (if (and (eq rime-prompt-tooltip 'posframe)
                   (not (string-match " *temp*" (buffer-name)))
                   (posframe-valid-p))
              (posframe-show rime-tooltip-posframe-buffer
                             :string prompt-str
                             :position (point)
                          :background-color (face-attribute 'rime-prompt :background)
                          :foreground-color (face-attribute 'rime-prompt :foreground))
              (message (propertize prompt-str 'face 'rime-prompt)))))))

(defun rime-prompt--minibuffer-message (string)
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

(defun rime-input-empty-p ()
  (= (buffer-size rime-input-buffer) 0))

;; add word to ~/.emacs.d/rime/my_phrase.dict.yaml and sort
(defun rime-add-to-user-dict ())

;; add word to ~/.emacs.d/rime/custom_phrase.txt and sort:
;; auto add pinyin code, prompt for confirmation 
(defun rime-add-to-custom-phrase ())

(defun rime-register-input-method ()
  (register-input-method "rime" "euc-cn" 'rime-activate "中"))

(add-hook 'emacs-startup-hook 'rime-register-input-method)
(rime-register-input-method)
(provide 'rime)

;;; rime.el ends here
