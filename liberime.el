;;; liberime.el --- Rime elisp binding

;; Author: A.I.
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; load liberime by default

;;; Code:

(require 'nadvice)
(require 'cl-lib)

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

(defun liberime-get-library-directory ()
  "Return the liberime package direcory."
  (file-name-directory
   (or (locate-library "liberime")
       (locate-library "liberime-config"))))

(defun liberime-open-directory (directory)
  "Open DIRECTORY with external app."
  (let ((directory (expand-file-name directory)))
    (cond ((string-equal system-type "windows-nt")
           (w32-shell-execute "open" directory))
          ((string-equal system-type "darwin")
           (concat "open " (shell-quote-argument directory)))
          ((string-equal system-type "gnu/linux")
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" directory))))))

(defun liberime-open-user-data-dir ()
  "Open user data dir with external app."
  (interactive)
  (liberime-open-directory liberime-user-data-dir))

(defun liberime-open-package-directory ()
  "Open liberime library directory with external app."
  (interactive)
  (liberime-open-directory (liberime-get-library-directory)))

(defun liberime-get-module-file ()
  "Return the path of liberime-core file."
  (when (liberime-get-library-directory)
    (concat (liberime-get-library-directory)
            "build/liberime-core"
            module-file-suffix)))

(defun liberime--load()
  (unless (featurep 'liberime-core)
    (load-file (liberime-get-module-file)))
  (unless (featurep 'liberime-core)
    (t (error "cannot load librime-core module")))
  (liberime--start))

(defun liberime-build ()
  (interactive)
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

(defun liberime-get-preedit ()
  "Get rime preedit."
  (let* ((context (liberime-get-context))
         (composition (alist-get 'composition context))
         (preedit (alist-get 'preedit composition)))
    preedit))

(defun liberime-get-page-size ()
  "Get rime page size from context."
  (let* ((context (liberime-get-context))
         (menu (alist-get 'menu context))
         (page-size (alist-get 'page-size menu)))
    page-size))

(defun liberime-select-candidate-crosspage (num)
  "Select rime candidate cross page.

This function is different from `liberime-select-candidate', When
NUM > page size, `liberime-select-candidate' do nothing, while
this function will go to proper page then select a candidate."
  (let* ((page-size (liberime-get-page-size))
         (position (- num 1))
         (page-n (/ position page-size))
         (n (% position page-size)))
    (liberime-process-key 65360) ;回退到第一页
    (dotimes (_ page-n)
      (liberime-process-key 65366)) ;发送翻页
    (liberime-select-candidate n)))

(defun liberime-clear-commit ()
  "Clear the lastest rime commit."
  ;; NEED IMPROVE: Second run `liberime-get-commit' will clear commit.
  (liberime-get-commit))

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
   ((file-exists-p (liberime-get-module-file)) (liberime--load))
   ((y-or-n-p "liberime-core must be built, do so now?") (liberime-build))
   (t (error "liberime-core not loaded"))))

(liberime-load)

(provide 'liberime)
