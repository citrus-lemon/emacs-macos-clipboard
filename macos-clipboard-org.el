;;; macos-clipboard-org.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'macos-clipboard-nspasteboard)

(defun macos-clipboard-org-kill-new (str)
  (kill-new str)
  (macos-clipboard-set-string (org-export-string-as str 'html t '(:with-toc nil)) "public.html"))

(defun macos-clipboard-org-copy-region-as-kill (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (let ((str (if region
                 (funcall region-extract-function nil)
               (filter-buffer-substring beg end))))
    (macos-clipboard-org-kill-new str))
  (setq deactivate-mark t)
  nil)

(eval-after-load 'org
  '(define-key org-mode-map (kbd "s-c") 'macos-clipboard-org-copy-region-as-kill))

(provide 'macos-clipboard-org)
;;; macos-clipboard-org.el ends here
