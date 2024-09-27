(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(let ((liberime-user-data-dir (locate-user-emacs-file "test/rime")))
  (load "liberime.el"))

(liberime-get-schema-list)
(liberime-select-schema "luna_pinyin_simp")
(liberime-select-schema "double_pinyin_flypy")

(liberime-search "nihao")

(measure-time
  (liberime-search "y" nil))
(measure-time (liberime-search "y" 10 3000))

(liberime-finalize)
(liberime-get-user-config "default.custom" "patch/menu/page_size" "int")
(liberime-set-user-config "default.custom" "patch/menu/page_size" 10 "int")
(liberime-get-schema-config "" "speller/auto_select" "bool")
(liberime-set-schema-config "" "speller/auto_select" nil "bool")
(liberime-sync-user-data)

(defun try-context()
  (liberime-clear-composition)
  (liberime-process-key (string-to-char "w"))
  (liberime-process-key (string-to-char "o"))
  (liberime-get-context))

(try-context)

(require 'pyim)
(setq pyim-default-scheme 'quanpin)
;; (setq pyim-default-scheme 'rime)
(set-input-method 'pyim)
