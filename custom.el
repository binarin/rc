(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/save/bookmarks")
 '(eval-expression-print-level 4)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "desktop.bundles" "{arch}" ".bem")))
 '(pt-executable "~/bin/pt")
 '(safe-local-variable-values
   (quote
    ((firestarter . binarin/tangle-and-bytecompile-current-buffer)
     (Syntax . ANSI-Common-Lisp)
     (Base . 10)
     (encoding . utf-8-unix)
     (encoding . utf-8)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 179 :width normal :foundry "unknown" :family "M+ 1mn"))))
 '(diff-refine-added ((t (:inherit diff-added :background "green4" :weight bold))))
 '(diff-refine-change ((t (:inherit diff-changed :background "yellow" :weight bold))))
 '(diff-refine-removed ((t (:inherit diff-removed :background "pink4" :foreground "grey" :weight bold))))
 '(hydra-face-blue ((t (:foreground "plum" :weight bold))))
 '(mode-line ((t (:background "#212931" :foreground "#eeeeec" :box (:line-width -1 :style released-button) :height 0.7))))
 '(mode-line-buffer-id ((t (:foreground "#f0dfaf" :weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5f7f5f" :box (:line-width -1 :style released-button) :height 0.7))))
 '(org-agenda-calendar-event ((t (:inherit org-time-grid))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "lime green"))) t)
 '(org-agenda-dimmed-todo-face ((t (:foreground "grey70"))))
 '(org-mode-line-clock ((t (:inherit mode-line))) t)
 '(pe/directory-face ((t (:inherit dired-directory :height 0.5))))
 '(pe/file-face ((t (:inherit default :height 0.5))))
 '(whitespace-line ((t (:background "dim gray" :foreground "#dc8cc3")))))
