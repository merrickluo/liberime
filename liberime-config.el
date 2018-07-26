(require 'nadvice)

(defvar liberime-shared-data-dir nil)
(defvar liberime-user-data-dir nil)
(defvar liberime-search-candidate-limit nil)

;;;###autoload
(defun liberime-config-load ()
  ;; dynamic module load
  (unless module-file-suffix
    (error "Module support not detected, liberime can't work"))

  (let* ((liberime-root (file-name-directory (or load-file-name buffer-file-name)))
         (module-file (concat liberime-root "build/liberime" module-file-suffix)))
    (load-file module-file))

  ;; param check
  (unless liberime-shared-data-dir
    ;; only guess on linux
    (if (and (string= system-type "gnu/linux")
             (file-directory-p "/usr/share/rime-data"))
        (setq liberime-shared-data-dir "/usr/share/rime-data")))
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

;; TODO how autoload work?
(liberime-config-load)

(provide 'liberime-config)
