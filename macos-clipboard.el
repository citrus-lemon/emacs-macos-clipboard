;;; macos-clipboard.el --- Communicate with macOS pasteboard server

;; Copyright (C) 2023 Hanyuan Li

;; Author: Hanyuan Li <lihanyuan1996@gmail.com>
;; URL: https://github.com/citrus-lemon/emacs-macos-clipboard
;; Keywords: clipboard macos
;; Version: 1.0.0

;;; Commentary:
;;
;; communicate with macOS pasteboard server
;;

;;; Code:

(require 'macos-clipboard-nspasteboard)

(defun macos-clipboard-extract-pasteboard (&optional types)
  (if types
      (if (eq types 'only-types)
          (macos-clipboard--extract-pasteboard-only-types)
        (macos-clipboard--extract-pasteboard-with-types (if (listp types) types (list types))))
    (macos-clipboard--extract-pasteboard)))

(defun macos-clipboard-set-string (value &optional type)
  (macos-clipboard--set-string value type))

(provide 'macos-clipboard)
;;; macos-clipboard.el ends here
