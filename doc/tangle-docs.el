;;; tangle-docs.el --- tangle janet src blocks -*- lexical-binding: t; -*-

;;; Commentary:
;;   Every doc/*.org and doc/topics/*.org in the library has a tangle target
;;   This file is used to generate janet files from the docs via 'jpm run docs'

;;; Code:
(require 'org)

(defvar tangle-docs--out "doc/topics.janet")
(defvar tangle-docs--dir "doc/examples")

(setq-default comment-start "#")

(defun tangle-docs--sources ()
  "All org sources with a janet tangle target."
  (append (list "doc/overview.org")
          (sort (mapcar (lambda (f) (concat "doc/topics/" f))
                        (delq nil
                              (mapcar (lambda (f)
                                        (and (string-suffix-p ".org" f) f))
                                      (directory-files
                                       "doc/topics" nil
                                       directory-files-no-dot-files-regexp t)))))))

(let ((sources (tangle-docs--sources))
      (written nil)
      (lbr (make-string 79 ?=)))
  (make-directory tangle-docs--dir t)
  (dolist (f sources)
    (unless (file-exists-p f)
      (error "tangle-docs: missing %s" f))
    (find-file f)
    (setq written (append written (or (org-babel-tangle) nil)))
    (kill-buffer))
  (let ((parts nil))
    (dolist (f written)
      (when (file-exists-p f)
        (push (concat (format "\n\n#%s\n#  file: %s\n#%s\n\n"
                              lbr (file-relative-name f) lbr)
                      (with-temp-buffer
                        (insert-file-contents f)
                        (buffer-string)))
              parts)))
    (with-temp-file tangle-docs--out
      (insert (apply #'concat (nreverse parts)))))
  (message "tangle-docs: %d files -> %s" (length written) tangle-docs--out))

(provide 'tangle-docs)
;;; tangle-docs.el ends here
