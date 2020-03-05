;;; liberime-config.el --- setup liberime automatically

;; Author: A.I.
;; Keywords: input method, rime
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; load liberime by default

;;; Code:

(if (require 'liberime nil t)
    (message "Liberime: please use (require 'liberime) instead of (require 'liberime-config).")
  (message "Liberime: (require 'liberime) failure, please check your quelpa config."))

(provide 'liberime-config)
