;;; .custom-set.el --- Holding the custom set variables
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; Custom set variables and faces messes up the main config and are
;; more manageable in a separate file

;;; Code:
(setq custom-file "~/.emacs.d/personal/custom-set.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" default))
 '(doc-view-continuous t)
 '(helm-completion-style 'helm)
 '(jedi:complete-on-dot t)
 '(jedi:tooltip-method '(popup))
 '(org-agenda-files nil)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (C . t)
     (sql . t)
     (haskell . t)
     (org . t)
     (python . t)
     (R . t)))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.4 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-pomodoro-keep-killed-pomodoro-time t)
 '(org-pomodoro-length 15)
 '(package-selected-packages
   '(gruber-darker auto-virtualenv htmlize projectile helm-bind-key haskell-mode rainbow-delimiters sphinx-doc gruber-darker-theme default-font-presets ess sml sml-mode company-jedi company-ycmd py-autopep8 async async-await lsp-mode auto-complete-config goto-last-change zenburn-theme yasnippet-snippets yasnippet-classic-snippets which-key virtualenv use-package undo-tree spaceline smartparens slime pdf-tools panda-theme org-pomodoro org magit jupyter iedit helm-xref helm-rg helm-flyspell flycheck-pyflakes expand-region elpy delight crux company-box benchmark-init auto-complete auctex ahungry-theme ace-window))
 '(pdf-view-display-size 'fit-page)
 '(pdf-view-use-imagemagick t)
 '(pdf-view-use-scaling t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 128 :width normal))))
 '(company-scrollbar-bg ((t (:background "#422643c34560"))) t)
 '(company-scrollbar-fg ((t (:background "#35a736f63845"))) t)
 '(company-tooltip ((t (:inherit default :background "#3327346735a6"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(org-block ((t (:inherit (fixed-pitch default) :extend t))))
 '(org-code ((t (:inherit (default fixed-pitch))))))
(provide 'custom-set)
;;; custom-set.el ends here
