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
  "Return the current macOS pasteboard contents.

If TYPES is nil, return all pasteboard items as a list of entries.
Each entry contains one or more (TYPE . DATA) pairs, where TYPE is a
Uniform Type Identifier (UTI) string (e.g., \"public.utf8-plain-text\")
and DATA is either a string or raw binary data.

If TYPES is a list of UTI strings, return only those types for each
item.  If TYPES is a single UTI string, it is treated as a one-element
list.

If TYPES is the symbol `only-types', return only the list of available
type strings (UTIs) for each pasteboard item.

Return nil if the pasteboard is empty or contains no matching types."
  (if types
      (if (eq types 'only-types)
          (macos-clipboard--extract-pasteboard-only-types)
        (let ((result (macos-clipboard--extract-pasteboard-with-types
                       (if (listp types) types (list types)))))
          ;; HACK: avoid result of '(nil)
          (if (equal result '(nil))
              nil
            result)))
    (macos-clipboard--extract-pasteboard)))

(defun macos-clipboard-set-string (value &optional type)
  "Set the macOS pasteboard text to VALUE.

VALUE must be a Lisp string.  It will be stored as UTF-8 text.

Optional TYPE specifies the pasteboard UTI to use
 (default: \"public.utf8-plain-text\")."
  (macos-clipboard--set-string value type))

(defun macos-clipboard-set-data (value type)
  "Set the macOS pasteboard binary data to VALUE.

VALUE must be a Lisp string containing the raw bytes to store.
Use `string-as-unibyte' if VALUE is a multibyte string.

TYPE is a UTI string that declares the data type
 (e.g., \"public.png\", \"public.file-url\")."
  (macos-clipboard--set-data (string-to-unibyte value) type))

(provide 'macos-clipboard)
;;; macos-clipboard.el ends here
