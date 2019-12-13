;;; liberime-config.el --- setup liberime automatically

;; Author: A.I.
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; load liberime by default

;;; Code:

(require 'nadvice)
(require 'cl-lib)

(defvar liberime-search-candidate-limit nil)

(defvar liberime--root
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar liberime--module-file
  (concat liberime--root "build/liberime" module-file-suffix))

(defcustom after-liberime-load-hook nil
  "List of functions to be called before quelpa."
  :group 'liberime
  :type 'hook)

(defcustom liberime-after-start-hook nil
  "List of functions to be called after liberime start"
  :group 'liberime
  :type 'hook)

(make-obsolete-variable 'after-liberime-load-hook 'liberime-after-start-hook "2019-12-13")

(defcustom liberime-shared-data-dir
  ;; only guess on linux
  (cl-case system-type
    ('gnu/linux
     (cl-some (lambda (parent)
                (let ((dir (expand-file-name "rime-data" parent)))
                  (when (file-directory-p dir)
                    dir)))
              (if (fboundp 'xdg-data-dirs)
                  (xdg-data-dirs)
                '("/usr/share/local" "/usr/share"))))
    ('darwin
     "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))
  "Data directory on the system."
  :group 'liberime
  :type 'file)

(defcustom liberime-user-data-dir
  (locate-user-emacs-file "rime/")
  "Data directory on the user home directory."
  :group 'liberime
  :type 'file)

(defun liberime--load()
  (unless (featurep 'liberime)
    (load-file liberime--module-file))
  (unless (featurep 'liberime)
    (t (error "cannot load librime")))
  (liberime--start)
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

(defun liberime--start ()
  (unless (or (and liberime-shared-data-dir
                   (file-directory-p liberime-shared-data-dir))
              (and liberime-user-data-dir
                   (file-directory-p liberime-user-data-dir)))
    (user-error "Please set liberime-shared-data-dir or liberime-user-data-dir"))

  ;; use liberime-search-candidate-limit if not provided
  (defun limited-liberime-search (search &rest arguments)
    (let ((pinyin (car arguments))
          (limit (or (cadr arguments) liberime-search-candidate-limit)))
      (apply search (list pinyin limit))))
  (advice-add #'liberime-search :around #'limited-liberime-search)

  (liberime-start liberime-shared-data-dir liberime-user-data-dir)
  (run-hooks 'liberime-after-start-hook))

(defun liberime-deploy()
  "redeploy liberime to affect config file change"
  (interactive)
  (liberime-finalize)
  (liberime--start))

(defun liberime-set-page-size (page-size)
  (liberime-set-config "default.custom" "patch/menu/page_size" 100 "int")
  (liberime-redeploy))

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
