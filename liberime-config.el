;;; liberime-config.el --- setup liberime automatically

;; Author: A.I.
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; load liberime by default

;;; Code:

(require 'nadvice)

(defvar liberime-shared-data-dir nil)
(defvar liberime-user-data-dir nil)
(defvar liberime-search-candidate-limit nil)

(defvar liberime--root
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar liberime--module-file
  (concat liberime--root "build/liberime" module-file-suffix))

(defcustom after-liberime-load-hook nil
  "List of functions to be called before quelpa."
  :group 'liberime
  :type 'hook)

(defun liberime--load()
  (unless (featurep 'liberime)
    (load-file liberime--module-file))
  (unless (featurep 'liberime)
    (t (error "cannot load librime")))
  (liberime--config)
  (run-hooks 'after-liberime-load-hook))

(defun liberime--build ()
  (let ((default-directory liberime--root))
    (set-process-sentinel
     (start-process "liberime-build" "*liberime build*" "make")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (= 0 (process-exit-status proc))
             (liberime--load)
           (pop-to-buffer "*liberime build*")
           (error "liberime: building failed with exit code %d" (process-exit-status proc))))))))

(defun liberime--config ()
  ;; param check
  (unless liberime-shared-data-dir
    ;; only guess on linux
    (cond ((and (string= system-type "gnu/linux")
               (file-directory-p "/usr/share/rime-data"))
           (setq liberime-shared-data-dir "/usr/share/rime-data"))
          ((and (string= system-type "darwin")
                (file-directory-p "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))
           (setq liberime-shared-data-dir "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))))
  (unless liberime-user-data-dir
    ;; defaults to ~/.emacs.d/rime
    (setq liberime-user-data-dir (expand-file-name (concat user-emacs-directory "rime/"))))
  (unless (or liberime-shared-data-dir liberime-user-data-dir)
      (error "Please set liberime-shared-data-dir and liberime-user-data-dir"))

  ;; use liberime-search-candidate-limit if not provided
  (defun limited-liberime-search (search &rest arguments)
    (let ((pinyin (car arguments))
          (limit (or (cadr arguments) liberime-search-candidate-limit)))
      (apply search (list pinyin limit))))
  (advice-add #'liberime-search :around #'limited-liberime-search)

  (liberime-start liberime-shared-data-dir liberime-user-data-dir))

(defun liberime-redeploy()
  "redeploy liberime to affect config file change"
  (interactive)
  (liberime-finalize)
  (liberime--config))

;;;###autoload
(defun liberime-load ()
  (interactive)
  (unless module-file-suffix
    (error "Module support not detected, liberime can't work"))
  (cond
   ((file-exists-p liberime--module-file) (liberime--load))
   ((y-or-n-p "liberime must be built, do so now?") (liberime--build))
   (t (error "liberime not loaded"))))

(liberime-load)

(provide 'liberime-config)
