;;; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; ;;

;;; Code:

;; ;; loading file containing personal information
(load-file "./.emacs.d/emacs_p.el")

;; removed keyboard shortcut to avoid accidentally killing emacs
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; avoid setting variables using customize
(setq custom-file (make-temp-file "emacs-custom"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)

(show-paren-mode 1)

;; wrap lines when in text modes.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq-default indent-tabs-mode -1)

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
  ;; Increase size of garbage collection during startup
  (setq gc-cons-threshold 10000000)

  ;; Restore after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 1000000)
              (message "gc-cons-threshold restored to %S"
                       gc-cons-threshold)))
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

  ;; Restore after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 1000000)
              (message "gc-cons-threshold restored to %S"
                       gc-cons-threshold)))

;; fast way to enter config file
(defun find-config ()
  "Find and modify config file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") 'find-config)

;; color theme
(use-package panda-theme
  :ensure t
  :config
  (load-theme 'panda t))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

(use-package iedit
  :ensure t
  :config
  (global-set-key (kbd "C-M-;") 'iedit))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;; ;; (use-package python
;; ;;   :mode ("\\.py\\'" . python-mode)
;; ;;         ("\\.wsgi$" . python-mode)
;; ;;   :interpreter ("python" . python-mode)

;; ;;   :init
;; ;;   (setq-default indent-tabs-mode nil)

;; ;;   :config
;; ;;   (add-hook 'python-mode-hook 'my/python-mode-hook)
;; ;;   (setq python-indent-offset 4)
;; ;;   (setq python-indent-guess-indent-offset-verbose nil)

;; ;;   (use-package elpy
;; ;;     :ensure t
;; ;;     :init
;; ;;     (elpy-enable)
;; ;;     ;; (add-hook 'python-mode-hook 'jedi:setup)
;; ;;     :config
;; ;;     (setq elpy-rpc-python-command "python3")
;; ;;     ;;(setq jedi:complete-on-dot t)
;; ;;     )
;; ;;   )

;; (use-package ace-window
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-p") 'ace-select-window)
;;   )

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-q") 'undo)
  (global-set-key (kbd "C-S-q") 'undo-tree-redo)
  (undo-tree-mode 1)
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  )

;; (use-package tex
;;   :ensure auctex
;;   :init
;;   (add-hook 'LaTeX-mode-hook (lambda ()
;;                                (TeX-fold-mode 1)))
;;   :config
;;   ;; to use pdfview with auctex
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;     TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;     TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;;   ;; TODO: fix errors at bottom of page.
;;   ;; to have the buffer refresh after compilation
;;   ;;(add-hook 'TeX-after-compilation-finished-functions
;;   ;;      #'TeX-revert-document-buffer)
;;   )

;; (use-package org
;;   :init
;;   (setq org-directory emacs_p-ORG_DIRECTORY)
;;   (setq org-agenda-files emacs_p-ORG_AGENDA_FILES)

;;   (global-set-key (kbd "C-c l") 'org-store-link)
;;   (global-set-key (kbd "C-c a") 'org-agenda)
;;   (global-set-key (kbd "C-c c") 'org-capture)

;;   (setq org-refile-targets
;;         '((nil :maxlevel . 3)
;;           (org-agenda-files :maxlevel . 3)))

;;   (setq org-support-shift-select t)

;;   (setq org-columns-default-format "%50ITEM(Task) %6CLOCKSUM %25TIMESTAMP_IA")

;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "STARTED(s)" "MEETING(m)"  "ISSUE(p)" "INPUTNEEDED(i)" "VERIFY(v)" "|" "SCOPECHANGE(r)" "DONE(d)" "CANCELLED(c)")))

;;   (setq org-tag-alist
;;         '(("fix" . ?f) ("message" . ?m) ("buy" . ?b) ("read" . ?r)))

;;   (add-hook 'org-mode-hook 'org-indent-mode)

;;   (setq org-capture-templates emacs_p-ORG_TEMPLATES)

;;   :config
;;   (use-package org-crypt
;;     :init
;;     (setq org-tags-exclude-from-inheritance (quote ("crypt")))
;;     (setq org-crypt-key nil)
;;     :config
;;     (org-crypt-use-before-save-magic)
;;     )
;;   (use-package epa-file
;;     :init
;;     (epa-file-enable)
;;     )
;;   )

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         ("C-c j" . magit-dispatch)
         ("C-c k" . magit-file-dispatch)
         ("C-c z" . magit-log-buffer-file)
         ("C-c b" . magit-blame))
  )

(use-package delight
  :ensure t)

(use-package helm
  :ensure t
  :delight
  :bind
  (("M-x"     . #'helm-M-x))
  (("C-x C-f" . #'helm-find-files))
  (("C-x C-b" . #'helm-buffers-list))
  :config
  (use-package helm-flyspell :after (helm flyspell))
  (use-package helm-xref)
  (use-package helm-rg)
  (require 'helm-config)
  (helm-mode t)
  (helm-autoresize-mode 1)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

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
  ;; (use-package color
  ;;   :init
  ;;   ;; small hack too avoid color scheme of company jedi colliding with color scheme of emacs
  ;;   (require 'color)
  ;;   (let ((bg (face-attribute 'default :background)))
  ;;     (custom-set-faces
  ;;      `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 4)))))
  ;;      `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
  ;;      `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
  ;;      `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
  ;;      `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))
  ;;     )
  ;;   )
  )

(use-package company-box
  :hook (company-mode . company-box-mode)
  )

;; (defun my/python-mode-hook ()
;;   "Strange function to make Jedi work with Company."
;;   (add-to-list 'company-backends 'company-jedi)
;;   )

(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  )

(use-package spaceline
  :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :ensure spaceline
  :config
  ;; (spaceline-helm-mode 1)
  (spaceline-emacs-theme))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.33)
  )

(provide 'init)
;;; init.el ends here
