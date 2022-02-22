;;; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; ;;

;;; Code:

;; color theme (load early to avoid blinking emacs window)qq
(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))
(require 'personal_information)
(require 'custom-set)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'startup)
(require 'org_config)
(require 'python_config)
(require 'web_dev)

(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      make-backup-files nil
      indent-tabs-mode -1)
(tool-bar-mode -1)

(setq-default tab-width 4
	      make-backup-files nil
	      indent-tabs-mode nil
	      show-trailing-whitespace t
	      visible-bell nil)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "RET") 'newline-and-indent)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-flycheck-mode)

(use-package equake
  :ensure t
  ;; some examples of optional settings follow:
  :custom
  ;; set width a bit less than full-screen (prevent 'overflow' on multi-monitor):
  (equake-size-width 0.99)
  ;; set distinct face for Equake: white foreground with dark blue background, and different font:
  :custom-face
  (equake-buffer-face
   ((t (:inherit 'default :family "DejaVu Sans Mono" :background "#000022" :foreground "white"))))
  :config
  ;; prevent accidental frame closure:
  (advice-add #'save-buffers-kill-terminal :before-while #'equake-kill-emacs-advice)
  ;; binding to restore last Equake tab when viewing a non-Equake buffer
  (global-set-key (kbd "C-M-^") #'equake-restore-last-etab)
  (global-set-key (kbd "C-z") 'equake-invoke)
  ;; set default shell
  (setq equake-default-shell 'shell)
  ;; set list of available shells
  (setq equake-available-shells
   '("shell"
     "vterm"
     "rash"
     "eshell")))

;; module for a nice collection of smart key bindings
(use-package crux
  :ensure t
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-k" . crux-smart-kill-line)
   ("C-c u" . crux-view-url)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("M-o" . crux-other-window-or-switch-buffer)
   ("C-c I" . crux-find-user-init-file))
)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; quickly marking/unmarking various structures of text
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
  ("C-M-=" . er/contract-region)))

;; package for smart handling of delimiters ("(","[", etc.)
(use-package smartparens
  :ensure t
  :config
  (show-smartparens-global-mode 1)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :bind
  (("M-(" . sp-wrap-round)
  ("M-[" . sp-wrap-square)
  ("M-{" . sp-wrap-curly)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package company
  :ensure t
  :delight
  :init
  (global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-tooltip-limit 30)
  (setq company-idle-delay .2)
  (setq company-echo-delay 0))

(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-select-window))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-q") 'undo)
  (global-set-key (kbd "C-S-q") 'undo-tree-redo)
  (undo-tree-mode 1))

(use-package sml-mode
  :ensure t
  :init
  (setenv "PATH" (concat "/usr/bin/sml" (getenv "PATH")))
  (setq exec-path (cons "/usr/bin/sml"  exec-path))
  )

(use-package tex
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (add-hook 'TeX-after-compilation-finished-functions
        #'TeX-revert-document-buffer))

(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c j" . magit-dispatch)
   ("C-c k" . magit-file-dispatch)
   ("C-c z" . magit-log-buffer-file)
   ("C-c b" . magit-blame)))

(use-package delight
  :ensure t)

(use-package helm
  :ensure t
  :delight
  :bind
  (("M-x"     . #'helm-M-x))
  (("C-x C-f" . #'helm-find-files))
  (("C-x C-b" . #'helm-buffers-list))
  :init
  (helm-mode t)
  :config
  (use-package helm-flyspell :after (helm flyspell))
  (use-package helm-xref)
  (use-package helm-rg)
  (helm-autoresize-mode 1)

  (setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame t
        helm-use-undecorated-frame-option t)

  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (set-face-attribute 'helm-selection nil
                    :background "purple"
                    :foreground "white"))

(use-package helm-spotify-plus
  :ensure t
  :config
  (global-set-key (kbd "C-c s s") 'helm-spotify-plus)  ;; s for SEARCH
  (global-set-key (kbd "C-c s f") 'helm-spotify-plus-next)
  (global-set-key (kbd "C-c s b") 'helm-spotify-plus-previous)
  (global-set-key (kbd "C-c s p") 'helm-spotify-plus-play)
  (global-set-key (kbd "C-c s g") 'helm-spotify-plus-pause) ;; g cause you know.. C-g stop things :)
)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "Aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package spaceline
  :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-toggle-minor-modes-off)
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.33))

(provide 'init)
;;; init.el ends here
