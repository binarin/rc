;; -*- lexical-binding: t -*-
(require 'cl)

(make-directory user-emacs-directory t)
(defvar binarin/primary-emacs-config (concat (file-name-directory (file-truename load-file-name))
                                             "emacs-config.org"))
(defvar binarin/tangled-emacs-config (concat user-emacs-directory "emacs-config.el"))

(setq gc-cons-threshold 20000000)

(setf prev (float-time))
(message "Starting %s" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))

(package-initialize)

(message "Package %s" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))
;; No sense in doing it again, we are already initialized.
;; And binarin/package-add-archive will do necessary housekeeping when
;; new archives are added.
(setf package-enable-at-startup nil)

(defun binarin/package-add-archive (name url)
  (add-to-list 'package-archives (cons name url) t)
  (package-read-archive-contents name)
  (unless (member name
                  (mapcar (lambda (archive) (package-desc-archive (cadr archive)))
                          package-archive-contents))
    (package--download-one-archive (cons name url) "archive-contents")
    (package-read-archive-contents name)))

;; Use org-mode with contrib from it's own elpa archive.

(if (file-exists-p "~/personal-workspace/org-mode/lisp/org.elc")
    (progn
      (eval-after-load "info"
        #'(lambda ()
            (add-to-list 'Info-directory-list "~/personal-workspace/org-mode/doc")))
      (add-to-list 'load-path "~/personal-workspace/org-mode/lisp")
      (require 'org))
  (binarin/package-add-archive "org" "http://orgmode.org/elpa/")
  (message "Org archive %s" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))
  (unless (package-installed-p 'org-plus-contrib)
    (package-install 'org-plus-contrib)))

(message "Org %s" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))

(when (file-newer-than-file-p binarin/primary-emacs-config binarin/tangled-emacs-config)
  (let ((block-counter 0))
    (cl-flet
        ((delete-empty-lines-at-buffer-start
          ()
          (save-excursion
            (beginning-of-buffer)
            (replace-regexp "\\`\\(\n\\|\\s-+\\)+" "")
            (save-buffer)))
         (profile-block
          ()
          (save-excursion
            (end-of-buffer)
            (insert "(message \"Block " (format "%s" (incf block-counter)) " %s\" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))"))))
      (require 'ob-tangle)
      (let ((org-babel-post-tangle-hook (cons #'delete-empty-lines-at-buffer-start org-babel-post-tangle-hook))
            ;; (org-babel-tangle-body-hook (cons #'profile-block org-babel-tangle-body-hook))
            )
        (message "Org pre-tangle %s" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))
        (org-babel-tangle-file binarin/primary-emacs-config binarin/tangled-emacs-config "emacs-lisp")))))

(load-file binarin/tangled-emacs-config)

(message "emacs-config %s" (let ((delta (- (float-time) prev))) (setf prev (float-time)) delta))
