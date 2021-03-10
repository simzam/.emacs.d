;;; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; 

;;; Code:

;; TODO: figure out a way to separate personal information into a separate file
(load-file "./.emacs.d/emacs_p.el")

;; removed keyboard shortcut to avoid accidentally killing emacs
(global-set-key (kbd "C-x C-c") 'delete-frame)

(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)

(load-theme 'wheatgrass)

;; wrap lines when in text modes.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; TODO
(require 'package)
(package-initialize)

(setq package-enable-at-startup nil)
(setq package-archives ())
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; ensures that packages used in this files are installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

;;
(use-package iedit
  :ensure t
  )

(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)

  :config
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil))


;; TODO fix python config
(use-package elpy
  :ensure t
  ;;:mode ("\\.py\\'" . python-mode)
  ;;         ("\\.wsgi$" . python-mode)
  :init
  (elpy-enable)

  :config
  (setq elpy-rpc-python-command "python3")
  )

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-select-window)
  )

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
  (global-flycheck-mode)
  )

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

  ;; TODO: fix errors at bottom of page.
  ;; to have the buffer refresh after compilation
  ;;(add-hook 'TeX-after-compilation-finished-functions
  ;;      #'TeX-revert-document-buffer)
  )

(use-package org
  :init
  (setq org-directory emacs_p-ORG_DIRECTORY)
  (setq org-agenda-files emacs_p-ORG_AGENDA_FILES)

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (setq org-support-shift-select t)

  (setq org-columns-default-format "%50ITEM(Task) %6CLOCKSUM %25TIMESTAMP_IA")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "MEETING(m)"  "ISSUE(p)" "INPUTNEEDED(i)" "VERIFY(v)" "|" "SCOPECHANGE(r)" "DONE(d)" "CANCELLED(c)")))

  (setq org-tag-alist
        '(("fix" . ?f) ("message" . ?m) ("buy" . ?b) ("read" . ?r)))

  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; TODO; update org-capture-templates
  (setq org-capture-templates emacs_p-ORG_TEMPLATES)
  ;;(eval-when-compile (defvar ORG_TEMPLATES)
  )

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         ("C-c j" . magit-dispatch)
         ("C-c k" . magit-file-dispatch)
         ("C-c z" . magit-log-buffer-file)
         ("C-c b" . magit-blame))
  )

;; TODO: read tutorial on delight
(use-package delight
  :ensure t
  )

;; TODO: read tutorial on helm
(use-package helm
  :ensure t
  :delight
  :bind (("M-x"     . #'helm-M-x))
  :bind (("C-x C-f" . #'helm-find-files))
  :bind (("C-x C-b" . #'helm-buffers-list))
  :config
  (use-package helm-flyspell :after (helm flyspell))
  (use-package helm-xref)
  (use-package helm-rg)
  (require 'helm-config)
  (helm-mode 1)
  )


;; TODO: read tutorial on yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  ;; TODO: fix error "might not be defined at runtime.
  ;;(yas-reload-all)
  )

(use-package yasnippet-snippets)

(use-package company
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
  )

(use-package company-box
  :hook (company-mode . company-box-mode)
  )

(defun my/python-mode-hook ()
  "Strange function to make Jedi work with Company."
  (add-to-list 'company-backends 'company-jedi)
  )

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

(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (edit slime let-alist pdf-tools org virtualenv zenburn-theme yasnippet-snippets yasnippet-classic-snippets use-package undo-tree magit jupyter helm-xref helm-rg helm-flyspell flycheck delight company-box auctex ahungry-theme ace-window))))
(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#0a3d0a3d0a3d"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
