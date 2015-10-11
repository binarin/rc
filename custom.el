(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bmkp-last-as-first-bookmark-file "/home/binarin/.emacs.d/save/bookmarks")
 '(custom-safe-themes
   (quote
    ("11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" default)))
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
 '(pt-executable "~/bin/pt")
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
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
 '(diff-refine-change ((t (:inherit diff-changed :background "yellow" :weight bold))) t)
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
 '(whitespace-line ((t (:background "dim gray" :foreground "#dc8cc3")))))
