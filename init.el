;;; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; ;;

;;; Code:

;; TODO: look over and clean up.
;; loading file containing personal information
(load-file "/home/sim/.emacs.d/org_p.el")

(setq custom-file (concat user-emacs-directory "org_p.el"))
(load custom-file 'noerror)

;; removed keyboard shortcut to avoid accidentally killing emacs
(global-set-key (kbd "C-x C-c") 'delete-frame)

(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)

;; reload file into the same buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;; wrap lines when in text modes.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; tabs are bad
(setq-default indent-tabs-mode -1)

;; trailing whitespace is bad
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'package)
(package-initialize)

(setq package-enable-at-startup nil)
(setq package-archives ())
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; ensures that packages used in this files are installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package benchmark-init
  :ensure t
  :init
  ;; increase size of garbage collection during startup
  (setq gc-cons-threshold 10000000)

  ;; restore after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 1000000)
              (message "gc-cons-threshold restored to %S"
                       gc-cons-threshold)))
  :config
  ;; to disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

  ;; restore after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 1000000)
              (message "gc-cons-threshold restored to %S"
                       gc-cons-threshold)))


(defvar emacs_p-find_dag "~/Desktop/.org/dag.org")
(defvar emacs_p-find_config "~/.emacs.d/init.el")



;; color theme
(use-package panda-theme
  :ensure t
  :config
  (load-theme 'panda t))

(use-package crux
  :ensure t
  :bind
  (("C-a" . crux-move-beginning-of-line)))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )


;; Package for quickly marking paragraphs, functions, or a other pieces of text
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; package for smart handling of delimiters ("(","[", etc.)
(use-package smartparens
  :ensure t
  :config
  (show-smartparens-global-mode +1)
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package company
  :ensure t
  :delight
  :init
  (global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; put most often used completions at stop of list
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-tooltip-limit 30)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (use-package color
    :init
    ;; small hack too avoid color scheme of company jedi colliding with color scheme of emacs
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 4)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))
      )
    )
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package elpy
  :ensure t
  :mode
  ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  (defun company-jedi-setup ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'company-jedi-setup)
  (elpy-enable)
  (use-package py-autopep8
    :ensure t)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup))

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

(use-package tex
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (TeX-fold-mode 1)))
  :config
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
        #'TeX-revert-document-buffer))

(use-package org
  :init
  ;; keep personal information out of config


  (setq org-agenda-files (list org-directory))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; depth in hierarchy to look for refile headlines
  (defvar org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (setq org-support-shift-select t)

  ;; TODO not started using
  ;; (setq org-columns-default-format "%50ITEM(Task) %6CLOCKSUM %25TIMESTAMP_IA")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "MEETING(m)"  "ISSUE(p)" "INPUTNEEDED(i)" "VERIFY(v)" "|" "SCOPECHANGE(r)" "DONE(d)" "CANCELLED(c)")))

  (setq org-tag-alist
        '(("fix" . ?f) ("message" . ?m) ("buy" . ?b) ("read" . ?r)))

  (add-hook 'org-mode-hook 'org-indent-mode)

  :config
  ;; scale 1.0 makes the math symbols hard to read.
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

  (defvar org-pomodoro-play-sounds nil)

  (use-package org-crypt
    :init
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq org-crypt-key nil)
    :config
    (org-crypt-use-before-save-magic))

  (use-package epa-file
    :init
    (epa-file-enable)))


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
  (require 'helm-config)
  (helm-autoresize-mode 1)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "aspell"
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
