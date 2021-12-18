;;; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; ;;

;;; Code:

;; color theme (load early to avoid blinking emacs window)
(load-theme 'gruber-darker t)

(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))
(require 'personal_information)
(require 'custom-set)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'startup)
(require 'org_config)
(require 'python_config)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      make-backup-files nil
      indent-tabs-mode -1)
(setq tool-bar-mode -1)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x C-c") 'delete-frame)
;; (global-set-key (kbd "RET") 'newline-and-indent)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; small hack to limit size of auto completion suggestions
(setq ac-max-width 0.5)

;; module for a nice collection of smart key bindings
(use-package crux
  :ensure t
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line)
  ("C-c u" . crux-view-url)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("M-o" . crux-other-window-or-switch-buffer)
  ("C-c I" . crux-find-user-init-file))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; quickly marking/unmarking various structures of text
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C-M-=" . er/contract-region))

;; package for smart handling of delimiters ("(","[", etc.)
(use-package smartparens
  :ensure t
  :config
  (show-smartparens-global-mode +1)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :bind
  ("M-(" . sp-wrap-round)
  ("M-[" . sp-wrap-square)
  ("M-{" . sp-wrap-curly))

(use-package company
  :ensure t
  :delight
  :init
  (global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; put most often used completions at top of list
  (setq company-transformers '(company-sort-by-occurrence))

  (setq company-tooltip-limit 30)
  (setq company-idle-delay .2)
  (setq company-echo-delay 0)
  )

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
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (set-face-attribute 'helm-selection nil
                    :background "purple"
                    :foreground "white"))

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
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.33))

(provide 'init)
;;; init.el ends here
