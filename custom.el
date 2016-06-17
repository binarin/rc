(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" default)))
 '(edts-inhibit-package-check t)
 '(edts-man-root "/home/binarin/.emacs.d/edts/doc/18.1")
 '(eval-expression-print-level 4)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "desktop.bundles" "{arch}" ".bem")))
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-notify-p t)
 '(haskell-process-args-cabal-repl (quote ("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")))
 '(haskell-process-args-ghci (quote ("-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "ghci-ng")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(hindent-style "chris-done")
 '(org-agenda-files
   (quote
    ("~/org/mirantis.org" "~/org/mira-cal.org" "/home/binarin/org/personal.org" "/home/binarin/org/refile.org" "/home/binarin/org/subscriptions.org" "/home/binarin/.rc/emacs-config.org" "/home/binarin/org/ference.org")))
 '(package-selected-packages
   (quote
    (cider nix-mode markdown-mode jammer zeal-at-point yaml-mode paredit evil-nerd-commenter elisp-slime-nav puppet-mode multiple-cursors expand-region helm-projectile persp-projectile god-mode edit-server smart-tab zenburn-theme flx-ido yasnippet web-mode vimish-fold magit js2-mode shm hindent haskell-mode auto-highlight-symbol eproject auto-complete erlang electric-operator corral projectile perspective helm pt bookmark+ mu4e-maildirs-extension htmlize ws-butler ace-jump-mode key-chord undo-tree hydra zoom-frm visual-fill-column smart-mode-line-powerline-theme rich-minority highlight-parentheses request req-package f anaphora)))
 '(safe-local-variable-values
   (quote
    ((nxml-child-indent . 2)
     (eval c-set-offset
           (quote arglist-close)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote ++))
     (eval c-set-offset
           (quote case-label)
           0)
     (eval c-set-offset
           (quote statement-case-open)
           0)
     (eval c-set-offset
           (quote substatement-open)
           0)
     (c-continued-statement-offset . 2)
     (c-indent-level . 2)
     (allout-layout . t)
     (time-stamp-active . t)
     (encoding . utf-8)
     (haskell-indent-spaces . 4)
     (haskell-indent-spaces . 2)
     (hindent-style . "chris-done")
     (hindent-style . "gibiansky")
     (hindent-style . "johan-tibell")
     (haskell-process-type . cabal-repl)
     (shm-lambda-indent-style . leftmost-parent))))
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-presentation-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-refine-added ((t (:inherit diff-added :background "green4" :weight bold))))
 '(diff-refine-changed ((t (:inherit diff-changed :background "yellow" :weight bold))))
 '(diff-refine-removed ((t (:inherit diff-removed :background "pink4" :foreground "grey" :weight bold))))
 '(hydra-face-blue ((t (:foreground "plum" :weight bold))))
 '(mode-line ((t (:background "#212931" :foreground "#eeeeec" :box (:line-width -1 :style released-button) :height 0.7))))
 '(mode-line-buffer-id ((t (:foreground "#f0dfaf" :weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5f7f5f" :box (:line-width -1 :style released-button) :height 0.7))))
 '(org-agenda-calendar-event ((t (:inherit org-time-grid))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "lime green"))))
 '(org-agenda-dimmed-todo-face ((t (:foreground "grey70"))))
 '(org-mode-line-clock ((t (:inherit mode-line))))
 '(pe/directory-face ((t (:inherit dired-directory :height 0.5))))
 '(pe/file-face ((t (:inherit default :height 0.5))))
 '(shm-current-face ((t (:background "#555555"))))
 '(shm-quarantine-face ((t (:inherit font-lock-error))))
 '(whitespace-line ((t (:background "dim gray" :foreground "#dc8cc3"))))
 '(woman-bold ((t (:inherit bold :foreground "orange")))))
