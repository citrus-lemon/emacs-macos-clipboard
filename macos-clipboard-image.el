;;; macos-clipboard-image.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 citrus-lemon
;;
;; Author: citrus-lemon <lihanyuan@gmail.com>
;; Maintainer: citrus-lemon <lihanyuan@gmail.com>
;; Created: March 11, 2023
;; Modified: March 11, 2023
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/WindProphet/macos-clipboard-image
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'macos-clipboard-nspasteboard)

(defvar macos-clipboard-image-type-uniform-type-identifiers-alist
  '(("public.png" . png)
    ("public.jpeg" . jpeg))
  "Alist of (UTI . IMAGE-TYPE) pairs used to identify image files")

(defun macos-clipboard-image-create-image-from-clipboard ()
  (let ((types macos-clipboard-image-type-uniform-type-identifiers-alist)
        image-data)
    (while
        (and
         types
         (not
          (let ((uti (caar types)))
            (setq image-data
                  (cdr-safe (assoc uti (car-safe (macos-clipboard-extract-pasteboard))))))))
      (setq types (cdr types)))
    (if types
        (create-image image-data (cdar types) t)
      (progn (message "Cannot find image in pasteboard") nil))))

(provide 'macos-clipboard-image)
;;; macos-clipboard-image.el ends here
