;;; liberime.el --- Rime elisp binding

;; Author: A.I.
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; load liberime by default

;;; Code:
(require 'cl-lib)

(defcustom liberime-after-start-hook nil
  "List of functions to be called after liberime start"
  :group 'liberime
  :type 'hook)

(make-obsolete-variable 'after-liberime-load-hook 'liberime-after-start-hook "2019-12-13")

(defcustom liberime-module-file nil
  "Liberime module file on the system.
When it is nil, librime will auto search module in many path,
more detail can be found in `liberime-get-module-file'."
  :group 'liberime
  :type 'file)

(defcustom liberime-shared-data-dir nil
  "Data directory on the system."
  :group 'liberime
  :type 'file)

(defcustom liberime-user-data-dir
  (locate-user-emacs-file "rime/")
  "Data directory on the user home directory."
  :group 'liberime
  :type 'file)

(defvar liberime-message
  "Liberime can not load properly, please check:
1. Does your emacs support dynamic module?
   a. Emacs should build with \"--with-modules\".
   b. Variable `module-file-suffix' should return non-nil.
2. Does liberime-core module compile and load properly?
   a. User should install librime, gcc and cmake,
      then build liberime-core module according to README.org,
      Shortcut: (liberime-open-package-readme)
   b. User can try (liberime-build) shortcut function.
   c. Function (liberime-workable-p) should return non-nil.
3. When liberime works, call (liberime-load) to load it."
  "The message which will be showed when `liberime-load' failure.")

(defun liberime-get-library-directory ()
  "Return the liberime package direcory."
  (file-name-directory
   (or (locate-library "liberime")
       (locate-library "liberime-config"))))

(defun liberime-find-rime-data (parent-dirs &optional names)
  "Find directories listed in NAMES from PARENT-DIRS,
if NAMES is nil, \"rime-data\" as fallback."
  (cl-some (lambda (parent)
             (cl-some (lambda (name)
                        (let ((dir (concat (file-name-as-directory parent) name)))
                          (when (file-directory-p dir)
                            dir)))
                      (or names '("rime-data"))))
           (if (fboundp 'xdg-data-dirs)
               `(,@parent-dirs ,@(xdg-data-dirs))
             parent-dirs)))

(defun liberime-get-shared-data-dir ()
  "Return user data directory"
  (or liberime-shared-data-dir
      ;; Guess
      (cl-case system-type
        ('gnu/linux
         (liberime-find-rime-data
          '("/usr/share/local" "/usr/share")))
        ('darwin
         "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
        ('windows-nt
         (liberime-find-rime-data
          (list
           (expand-file-name
            (concat (file-name-directory (executable-find "emacs"))
                    "../share"))
           "c:/" "d:/" "e:/" "f:/" "g:/")
          '("rime-data"
            "msys32/mingw32/share/rime-data"
            "msys64/mingw64/share/rime-data"))))
      ;; Fallback to user data dir.
      (liberime-get-user-data-dir)))

(defun liberime-get-user-data-dir ()
  "Return user data directory, create it if necessary."
  (let ((directory (expand-file-name liberime-user-data-dir)))
    (unless (file-directory-p directory)
      (make-directory directory))
    directory))

(defun liberime-open-directory (directory)
  "Open DIRECTORY with external app."
  (let ((directory (expand-file-name directory)))
    (when (file-directory-p directory)
      (cond ((string-equal system-type "windows-nt")
             (w32-shell-execute "open" directory))
            ((string-equal system-type "darwin")
             (concat "open " (shell-quote-argument directory)))
            ((string-equal system-type "gnu/linux")
             (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" directory)))))))

;;;###autoload
(defun liberime-open-user-data-dir ()
  "Open user data dir with external app."
  (interactive)
  (liberime-open-directory (liberime-get-user-data-dir)))

;;;###autoload
(defun liberime-open-shared-data-dir ()
  "Open shared data dir with external app."
  (interactive)
  (liberime-open-directory (liberime-get-shared-data-dir)))

;;;###autoload
(defun liberime-open-package-directory ()
  "Open liberime library directory with external app."
  (interactive)
  (liberime-open-directory (liberime-get-library-directory)))

;;;###autoload
(defun liberime-open-package-readme ()
  "Open liberime library README.org."
  (interactive)
  (find-file (concat (liberime-get-library-directory) "README.org")))

(defun liberime-get-module-file ()
  "Return the path of liberime-core file."
  (let ((file1 (concat (liberime-get-library-directory)
                       "build/liberime-core"
                       module-file-suffix))
        (file2 (concat (file-name-directory
                        (or (executable-find "emacs")
                            "/usr/bin/emacs"))
                       "liberime-core" module-file-suffix)))
    (or liberime-module-file
        (when (file-exists-p file1) file1)
        (when (file-exists-p file2) file2)
        (locate-library "liberime-core")
        (locate-file
         (concat "liberime-core" module-file-suffix)
         exec-path))))

;;;###autoload
(defun liberime-build ()
  (interactive)
  (message "Liberime: start build liberime-core module ...")
  (let ((default-directory (liberime-get-library-directory)))
    (set-process-sentinel
     (start-process "liberime-build" "*liberime build*" "make")
     (lambda (proc _event)
       (when (eq 'exit (process-status proc))
         (if (= 0 (process-exit-status proc))
             (progn (liberime-load)
                    (message "Liberime: load liberime-core module successful."))
           (pop-to-buffer "*liberime build*")
           (error "liberime: building failed with exit code %d" (process-exit-status proc))))))))

(defun liberime-workable-p ()
  "Return t when liberime can work."
  (featurep 'liberime-core))

(defun liberime--start ()
  (let ((shared-dir (liberime-get-shared-data-dir))
        (user-dir (liberime-get-user-data-dir)))
    (message "Liberime: start with shared dir %S, user dir: %S" shared-dir user-dir)
    (liberime-start shared-dir user-dir)
    (run-hooks 'liberime-after-start-hook)))

;;;###autoload
(defun liberime-load ()
  (interactive)
  (ignore-errors
    (when (and module-file-suffix
               (liberime-get-module-file))
      (unless (featurep 'liberime-core)
        (load-file (liberime-get-module-file)))))
  (if (not (featurep 'liberime-core))
      (when (> (length liberime-message) 0)
        (let ((buf (get-buffer-create "*liberime message*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert liberime-message)
            (goto-char (point-min)))
          (pop-to-buffer buf)))
    (liberime--start)))

(liberime-load)

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

;;;###autoload
(defun liberime-deploy()
  "deploy liberime to affect config file change"
  (interactive)
  (liberime-finalize)
  (liberime--start))

;;;###autoload
(defun liberime-set-page-size (page-size)
  "set rime page-size to `prefix' or by default 100
example C-u 200 M-x liberime-set-page-size
you also need to call liberime-deploy to make it take affect
you only need to do this once.
"
  (interactive "P")
  (liberime-set-user-config "default.custom" "patch/menu/page_size" (or page-size 100) "int"))

;;;###autoload
(defun liberime-select-schema-interactive ()
  "Select a rime schema interactive."
  (interactive)
  (let ((schema-list
         (mapcar (lambda (x)
                   (cons (format "%s(%s)" (cadr x) (car x))
                         (car x)))
                 (ignore-errors (liberime-get-schema-list)))))
    (if schema-list
        (let* ((schema-name (completing-read "Rime schema: " schema-list))
               (schema (alist-get schema-name schema-list nil nil #'equal)))
          (liberime-select-schema schema)
          (message "Liberime: select %s schema." schema-name))
      (message "Liberime: schema %S is not found, ignore." schema))))

;;;###autoload
(defun liberime-sync ()
  "sync rime user data
you should specify sync_dir in ~/.emacs.d/rime/installation.yaml
"
  (interactive)
  (liberime-sync-user-data))

(provide 'liberime)
;;; liberime.el ends here
