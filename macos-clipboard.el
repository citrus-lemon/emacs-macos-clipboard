;;; emacs-clipboard.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
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
