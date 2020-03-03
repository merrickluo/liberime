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
     "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
    ('windows-nt
     (expand-file-name "build/data" liberime--root)))
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
  (liberime-start liberime-shared-data-dir liberime-user-data-dir)
  (run-hooks 'liberime-after-start-hook))

(defun liberime-deploy()
  "deploy liberime to affect config file change"
  (interactive)
  (liberime-finalize)
  (liberime--start))

(defun liberime-set-page-size (page-size)
  "set rime page-size to `prefix' or by default 100
example C-u 200 M-x liberime-set-page-size
you also need to call liberime-deploy to make it take affect
you only need to do this once.
"
  (interactive "P")
  (liberime-set-user-config "default.custom" "patch/menu/page_size" (or page-size 100) "int"))

(defun liberime-sync ()
  "sync rime user data
you should specify sync_dir in ~/.emacs.d/rime/installation.yaml
"
  (interactive)
  (liberime-sync-user-data))

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
