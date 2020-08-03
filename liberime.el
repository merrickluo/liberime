;;; liberime.el --- Rime elisp binding    -*- lexical-binding: t; -*-

;; Author: A.I.
;; URL: https://github.com/merrickluo/liberime
;; Version: 0.0.6
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, Chinese, input-method, rime

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A Emacs dynamic module provide librime bindings for Emacs.

;;; Code:
(require 'cl-lib)

(defcustom liberime-after-start-hook nil
  "List of functions to be called after liberime start."
  :group 'liberime
  :type 'hook)

(make-obsolete-variable 'after-liberime-load-hook 'liberime-after-start-hook "2019-12-13")

(defcustom liberime-module-file nil
  "Liberime module file on the system.
When it is nil, librime will auto search module in many path."
  :group 'liberime
  :type 'file)

(defcustom liberime-shared-data-dir nil
  "Data directory on the system.

More info: https://github.com/rime/home/wiki/SharedData"
  :group 'liberime
  :type 'file)

(defcustom liberime-user-data-dir
  (locate-user-emacs-file "rime/")
  "Data directory on the user home directory."
  :group 'liberime
  :type 'file)

(defcustom liberime-auto-build nil
  "If set to t, try build when module file not found in the system."
  :group 'liberime
  :type 'boolean)

(defvar liberime-select-schema-timer nil
  "Timer used by `liberime-select-schema'.")

(defvar liberime-current-schema nil
  "The rime schema set by `liberime-select-schema'.")

(declare-function liberime-clear-composition "ext:src/liberime-core.c")
(declare-function liberime-commit-composition "ext:src/liberime-core.c")
(declare-function liberime-finalize "ext:src/liberime-core.c")
(declare-function liberime-get-commit "ext:src/liberime-core.c")
(declare-function liberime-get-context "ext:src/liberime-core.c")
(declare-function liberime-get-input "ext:src/liberime-core.c")
(declare-function liberime-get-schema-config "ext:src/liberime-core.c")
(declare-function liberime-get-schema-list "ext:src/liberime-core.c")
(declare-function liberime-get-status "ext:src/liberime-core.c")
(declare-function liberime-get-sync-dir "ext:src/liberime-core.c")
(declare-function liberime-get-user-config "ext:src/liberime-core.c")
(declare-function liberime-process-key "ext:src/liberime-core.c")
(declare-function liberime-search "ext:src/liberime-core.c")
(declare-function liberime-select-candidate "ext:src/liberime-core.c")
(declare-function liberime-select-schema "ext:src/liberime-core.c")
(declare-function liberime-set-schema-config "ext:src/liberime-core.c")
(declare-function liberime-set-user-config "ext:src/liberime-core.c")
(declare-function liberime-start "ext:src/liberime-core.c")
(declare-function liberime-sync-user-data "ext:src/liberime-core.c")

(defun liberime-get-library-directory ()
  "Return the liberime package direcory."
  (let ((file (or (locate-library "liberime")
                  (locate-library "liberime-config"))))
    (when (and file (file-exists-p file))
      (file-name-directory file))))

(defun liberime-find-rime-data (parent-dirs &optional names)
  "Find directories listed in NAMES from PARENT-DIRS.

if NAMES is nil, \"rime-data\" as fallback."
  (cl-some (lambda (parent)
             (cl-some (lambda (name)
                        (let ((dir (concat (file-name-as-directory parent) name)))
                          (when (file-directory-p dir)
                            dir)))
                      (or names '("rime-data"))))
           (remove nil (if (fboundp 'xdg-data-dirs)
                           `(,@parent-dirs ,@(xdg-data-dirs))
                         parent-dirs))))

(defun liberime-get-shared-data-dir ()
  "Return user data directory."
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
           (let ((file (executable-find "emacs")))
             (when (and file (file-exists-p file))
               (expand-file-name
                (concat (file-name-directory file)
                        "../share"))))
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

(declare-function w32-shell-execute "w32fns")

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

;;;###autoload
(defun liberime-build ()
  "Build liberime-core module."
  (interactive)
  (let ((buffer (get-buffer-create "*liberime build help*"))
        (dir (liberime-get-library-directory)))
    (if (not (and dir (file-directory-p dir)))
        (message "Liberime: library directory is not found.")
      (message "Liberime: start build liberime-core module ...")
      (with-current-buffer buffer
        (erase-buffer)
        (insert "* Liberime build help")
        (unless module-file-suffix
          (insert "** Your emacs do not support dynamic module.\n"))
        (unless (executable-find "gcc")
          (insert "** You should install gcc."))
        (unless (executable-find "make")
          (insert "** You should install make.")))
      (let ((default-directory dir)
            (makefile
             (concat
              (if (eq system-type 'windows-nt)
                  "LIBRIME = -llibrime\n"
                "LIBRIME = -lrime\n")
              (concat
               "CC = gcc\n"
               "LDFLAGS = -shared\n"
               "SRC = src\n"
               "SOURCES = $(wildcard $(SRC)/*.c)\n"
               "OBJS = $(patsubst %.c, %.o, $(SOURCES))\n")
              (format "TARGET = $(SRC)/liberime-core%s\n" (or module-file-suffix ".so"))
              (let* ((path (replace-regexp-in-string
                            "/share/emacs/.*" ""
                            (or (locate-library "files") "/usr")))
                     (include-dir (concat (file-name-as-directory path) "include/")))
                (if (file-exists-p (concat include-dir "emacs-module.h"))
                    (concat "CFLAGS = -fPIC -O2 -Wall -I " include-dir "\n")
                  (concat "CFLAGS = -fPIC -O2 -Wall -I emacs-module/" (number-to-string emacs-major-version) "\n")))
              (let ((p (getenv "RIME_PATH")))
                (if p
                    (concat "CFLAGS += -I " p "/src/\n"
                            "LDFLAGS += -L " p "/build/lib/ -L " p "/build/lib/Release/\n"
                            "LDFLAGS += -Wl,-rpath," p "/build/lib/:" p "/build/lib/Release\n")
                  "\n"))
              (concat
               ".PHONY:all objs\n"
               "all:$(TARGET)\n"
               "objs:$(OBJS)\n"
               "$(TARGET):$(OBJS)\n"
               "	$(CC) $(OBJS) $(LDFLAGS) $(LIBRIME) $(LIBS) -o $@"))))
        (with-temp-buffer
          (insert makefile)
          (write-region (point-min) (point-max) (concat dir "Makefile-liberime-build") nil :silent))
        (set-process-sentinel
         (start-process "liberime-build" "*liberime build*"
                        "make" "liberime-build")
         (lambda (proc _event)
           (when (eq 'exit (process-status proc))
             (if (= 0 (process-exit-status proc))
                 (progn (liberime-load)
                        (message "Liberime: load liberime-core module successful."))
               (pop-to-buffer buffer)
               (error "Liberime: building failed with exit code %d" (process-exit-status proc))))))))))

(defun liberime-workable-p ()
  "Return t when liberime can work."
  (featurep 'liberime-core))

(defun liberime--start ()
  "Start liberime."
  (let ((shared-dir (liberime-get-shared-data-dir))
        (user-dir (liberime-get-user-data-dir)))
    (message "Liberime: start with shared dir %S, user dir: %S" shared-dir user-dir)
    (liberime-start shared-dir user-dir)
    (when liberime-current-schema
      (liberime-try-select-schema liberime-current-schema))
    (run-hooks 'liberime-after-start-hook)))

;;;###autoload
(defun liberime-load ()
  "Load liberime-core module."
  (interactive)
  (when (and liberime-module-file
             (file-exists-p liberime-module-file)
             (not (featurep 'liberime-core)))
    (load-file liberime-module-file))
  (let* ((libdir (liberime-get-library-directory))
         (load-path
          (list libdir
                (concat libdir "src")
                (concat libdir "build"))))
    (require 'liberime-core nil t))
  (if (featurep 'liberime-core)
      (liberime--start)
    (if liberime-auto-build
        (liberime-build)
      (let ((buf (get-buffer-create "*liberime load*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "Liberime: Fail to load liberime-core module, try to run command: (liberime-build)")
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

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
  "Deploy liberime to affect config file change."
  (interactive)
  (liberime-finalize)
  (liberime--start))

;;;###autoload
(defun liberime-set-page-size (page-size)
  "Set rime page-size to PAGE-SIZE or by default 10.
you also need to call `liberime-deploy' to make it take affect
you only need to do this once."
  (interactive "P")
  (liberime-set-user-config "default.custom" "patch/menu/page_size" (or page-size 10) "int"))

(defun liberime-try-select-schema (schema_id)
  "Try to select rime schema with SCHEMA_ID."
  (let ((n 1))
    (setq liberime-current-schema schema_id)
    (when (featurep 'liberime-core)
      (when liberime-select-schema-timer
        (cancel-timer liberime-select-schema-timer))
      (setq liberime-select-schema-timer
            (run-with-timer
             1 2
             (lambda ()
               (let ((id (alist-get 'schema_id (ignore-errors (liberime-get-status)))))
                 (cond ((or (equal id schema_id)
                            (> n 10))
                        (if (> n 10)
                            (message "Liberime: fail to select schema %S." schema_id)
                          (message "Liberime: success to select schema %S." schema_id))
                        (message "")
                        (cancel-timer liberime-select-schema-timer)
                        (setq liberime-select-schema-timer nil))
                       (t (message "Liberime: try (n=%s) to select schema %S ..." n schema_id)
                          (ignore-errors (liberime-select-schema schema_id))))
                 (setq n (+ n 1))))))
      t)))

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
          (liberime-try-select-schema schema))
      (message "Liberime: no schema has been found, ignore."))))

;;;###autoload
(defun liberime-sync ()
  "Sync rime user data.
User should specify sync_dir in installation.yaml file of
`liberime-user-data-dir' directory."
  (interactive)
  (liberime-sync-user-data))

(provide 'liberime)

;;; liberime.el ends here
