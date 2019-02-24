;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(require 'cl)

(make-directory user-emacs-directory t)

(defvar binarin/primary-emacs-config
  (concat (file-name-directory (file-truename load-file-name)) "emacs-config.org"))

(defvar binarin/tangled-emacs-config
  (concat user-emacs-directory "emacs-config.el"))

(setq gc-cons-threshold 20000000)

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
            (end-of-buffer))))
      (require 'ob-tangle)
      (let ((org-babel-post-tangle-hook (cons #'delete-empty-lines-at-buffer-start org-babel-post-tangle-hook)))
        (org-babel-tangle-file binarin/primary-emacs-config binarin/tangled-emacs-config "emacs-lisp")))))

(unless init-file-user (setf init-file-user "")) ;; I'm testing with '-q', fake it (for amx mostly)

(load-file binarin/tangled-emacs-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-select-method
   (quote
    (nnmaildir "/home/binarin/.mail/binarin@binarin.ru")))
 '(safe-local-variable-values
   (quote
    ((intero-targets "haskell-restish-todo:lib" "haskell-restish-todo:exe:haskell-restish-todo-exe" "haskell-restish-todo:test:unit")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
