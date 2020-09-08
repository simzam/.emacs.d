;;; init file ==="config file for emacs.

(setq user-full-name "Simon Iversen")
(setq user-mail-address "simon.iversen@protonmail.com")

;;; Code:
(global-set-key (kbd "C-x C-c") 'delete-frame)

(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)


;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'package)                      ;
(package-initialize)

(setq package-enable-at-startup nil)
(setq package-archives ())
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(defvar my-auto-save-dir
  (file-name-as-directory (expand-file-name
			   "autosave" user-emacs-directory)))
(unless (file-exists-p my-auto-save-dir)
  (make-directory my-auto-save-dir))

(add-to-list 'auto-save-file-name-transforms
             (list "\\(.+/\\)*\\(.*?\\)" (expand-file-name "\\2" my-auto-save-dir))
             t)

;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(setq backup-by-copying t
      backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 3
      version-control t)

(load-theme 'misterioso)

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3"))

(use-package ace-window)
(global-set-key (kbd "M-p") 'ace-window)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(use-package undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-S-q") 'undo-tree-redo)
(undo-tree-mode 1)

(use-package flycheck
  :init
  (global-flycheck-mode))


;(use-package auctex)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))


(use-package org
  :init
  (setq org-directory "~/Desktop")
  (setq org-agenda-files '("~/Desktop/.org"))

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

  (setq org-capture-templates
	'(("t" "TODO" entry (file ".org/week.org")
	   "* TODO %? %^G \n  %U" :empty-lines 1)
	  ("s" "Scheduled TODO" entry (file ".org/week.org")
	   "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
	  ("d" "Deadline" entry (file ".org/week.org")
	   "* TODO %? %^G \n DEADLINE: %^t" :empty-lines 1)
	  ("p" "Priority" entry (file ".org/week.org")
	   "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
	  ("n" "Note" entry (file+headline ".org/.gen.org")
	   "* %? %^G\n%U" :empty-lines 1)
	  ("j" "Journal" entry (file+datetree ".org/.p.org")
	   "* %? %^G\nEntered on %U\n"))
	)
  )


(use-package magit
  :bind (("C-x g" . magit-status)))


(use-package delight)


(use-package helm
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


(use-package yasnippet
  :config
  (yas-global-mode)
  (yas-reload-all))


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
  :hook (company-mode . company-box-mode))


(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))


(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil))


(use-package jupyter)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/home/zam/Desktop/.org/project_euler.org" "/home/zam/Desktop/.org/concrete_mathematics.org" "/home/zam/Desktop/.org/coursera.org" "/home/zam/Desktop/.org/handle.org" "/home/zam/Desktop/.org/orgmode_tutorial.org" "/home/zam/Desktop/.org/sykkel.org" "/home/zam/Desktop/.org/velleman.org" "/home/zam/Desktop/.org/week.org" "/home/zam/Desktop/.org/year.org")))
 '(package-selected-packages
   (quote
    (virtualenv zenburn-theme yasnippet-snippets yasnippet-classic-snippets use-package undo-tree magit jupyter helm-xref helm-rg helm-flyspell flycheck delight company-box auctex ahungry-theme ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
