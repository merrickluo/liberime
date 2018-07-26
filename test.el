(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(require 'liberime)
(liberime-start "/usr/share/rime-data" (expand-file-name "~/.emacs.d/rime"))
(liberime-get-schema-list)
(liberime-select-schema "luna_pinyin_simp")
(liberime-search "wode" nil)


(require 'pyim)
(setq pyim-default-scheme 'quanpin)
;; (setq pyim-default-scheme 'rime)
(set-input-method 'pyim)
