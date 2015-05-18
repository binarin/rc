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
    ("282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" default)))
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
