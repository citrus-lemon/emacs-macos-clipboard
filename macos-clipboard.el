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
        (let ((result (macos-clipboard--extract-pasteboard-with-types (if (listp types) types (list types)))))
          ;; HACK: avoid result of '(nil)
          (if (equal result '(nil))
              nil
            result)))
    (macos-clipboard--extract-pasteboard)))

(defun macos-clipboard-set-string (value &optional type)
  (macos-clipboard--set-string value type))

(defun macos-clipboard-set-data (value type)
  (macos-clipboard--set-data (string-to-unibyte value) type))

(provide 'macos-clipboard)
;;; macos-clipboard.el ends here
