;;; config file for emacs.
;;; last updated: Tue Feb 16 16:27:14 2021

(setq user-full-name "Simon Iversen")
(setq user-mail-address "simon.iversen@protonmail.com")

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

(require 'package)
(package-initialize)

(setq package-enable-at-startup nil)
(setq package-archives ())
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


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
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
        #'TeX-revert-document-buffer)
  )


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


;; TODO: Setup magit
;; ;; essentials
;; (global-set-key (kbd "s-m m") 'magit-status)
;; (global-set-key (kbd "s-m j") 'magit-dispatch)
;; (global-set-key (kbd "s-m k") 'magit-file-dispatch)
;; ;; a faster way to invoke very common commands
;; (global-set-key (kbd "s-m l") 'magit-log-buffer-file)
;; (global-set-key (kbd "s-m b") 'magit-blame)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  )



(use-package delight
  :ensure t
  )

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

(use-package yasnippet
  :ensure t
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
  :hook (company-mode . company-box-mode)
  )

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))


;; small hack too avoid color scheme of company jedi colliding with color scheme of emacs
 (require 'color)
  
 (let ((bg (face-attribute 'default :background)))
   (custom-set-faces
    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 4)))))
    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/home/zam/Desktop/.org/project_euler.org" "/home/zam/Desktop/.org/handle.org" "/home/zam/Desktop/.org/orgmode_tutorial.org" "/home/zam/Desktop/.org/sykkel.org" "/home/zam/Desktop/.org/week.org")))
 '(package-selected-packages
   (quote
    (edit slime let-alist pdf-tools org virtualenv zenburn-theme yasnippet-snippets yasnippet-classic-snippets use-package undo-tree magit jupyter helm-xref helm-rg helm-flyspell flycheck delight company-box auctex ahungry-theme ace-window))))
