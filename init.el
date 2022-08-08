; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; ;; TODO introduce evil package LATER

;;; Code:
;;  configures main functionality and loads external config files

;; Add config files from separate folder into load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives ())
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; Ensures that use-package is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(use-package auto-package-update
  ;; Keeps the packages updated
  :ensure t
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;;(load-library "org_config")
;;(load-library "python_config")
;; (load-library "web_dev")
;;(load-library "latex_config")
;;(load-library "personal_information")
;;(load-library "custom-set")

(use-package gruber-darker-theme
  ;; color theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(setq-default inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      make-backup-files nil
      indent-tabs-mode -1
      tab-width 4
	  make-backup-files nil
	  indent-tabs-mode nil
	  show-trailing-whitespace t
	  visible-bell nil
      calendar-week-start-day 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Tool bar waste space
(tool-bar-mode -1)

;; Indent when pressing RET
(global-set-key (kbd "RET") 'newline-and-indent)
;; Trailing whitespace are never welcome
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Wrap text if text moves outside frame width
(global-visual-line-mode 1)

;; Highlight current line in active windoes
(global-hl-line-mode 1)

;; Always show line number for all frames
(global-linum-mode)

(use-package crux
  ;; Provides clean shortcuts for common tasks
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

;; TODO use in for quick region selection latex python
(use-package expand-region
  ;; quickly mark/unmark regions of text
  :ensure t
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

;; TODO: add package
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode +1)
;;   :bind (:map projectile-mode-map
;;               ("s-p" . projectile-command-map)
;;               ("C-c p" . projectile-command-map)))

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

;; TODO fix keybindings and sp pair nil?
(use-package smartparens
  ;; package for smart handling and navigation of delimiters ("(","[", etc.)
  :ensure t
  :init
  (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
  (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
  (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
  (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
  (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
  (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
  (bind-key "C-S-s" #'sp-splice-sexp)
  (bind-key "C-M-<backspace>" #'backward-kill-sexp)
  (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

  :config
  (smartparens-global-mode t)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (setq sp-highlight-pair-overlay nil)

  :bind
  (("M-(" . sp-wrap-round)
  ("M-[" . sp-wrap-square)
  ("M-{" . sp-wrap-curly)))

(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  (setq-default pdf-view-display-size 'fit-page))


;; TODO how does
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
  ;; :config
  ;; TODO check this
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :config
  (with-eval-after-load 'undo-tree
    (setq undo-tree-auto-save-history nil)))

(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c j" . magit-dispatch)
   ("C-c k" . magit-file-dispatch)
   ("C-c z" . magit-log-buffer-file)
   ("C-c b" . magit-blame)))

;; TODO what does this do
(use-package delight :ensure t)

;; TODO configure helm
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
  ;; (use-package helm-flyspell :after (helm flyspell))
  (use-package helm-xref :ensure t)
  (use-package helm-rg :ensure t)
  (helm-autoresize-mode 1)
  (setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-use-undecorated-frame-option t)
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

(use-package helm-spotify-plus
  ;; Control Spotify from Emacs
  :ensure t
  :bind
  (("C-c s s" . helm-spotify-plus)
   ("C-c s f" . helm-spotify-plus-next)
   ("C-c s b" . helm-spotify-plus-previous)
   ("C-c s p" . helm-spotify-plus-play)
   ("C-c s g" . helm-spotify-plus-pause)))

;; (use-package equake
;;   :ensure t
;;   ;; some examples of optional settings follow:
;;   :custom
;;   ;; set width a bit less than full-screen (prevent 'overflow' on multi-monitor):
;;   (equake-size-width 0.99)
;;   ;; set distinct face for Equake: white foreground with dark blue background, and different font:
;;   :custom-face
;;   (equake-buffer-face
;;    ((t (:inherit 'default :family "DejaVu Sans Mono" :background "#000022" :foreground "white"))))
;;   :config
;;   ;; prevent accidental frame closure:
;;   (advice-add #'save-buffers-kill-terminal :before-while #'equake-kill-emacs-advice)
;;   ;; binding to restore last Equake tab when viewing a non-Equake buffer
;;   (global-set-key (kbd "C-M-^") #'equake-restore-last-etab)
;;   (global-set-key (kbd "C-z") 'equake-invoke)
;;   ;; set default shell
;;   (setq equake-default-shell 'shell)
;;   ;; set list of available shells
;;   (setq equake-available-shells
;;    '("shell"
;;      "vterm"
;;      "rash"
;;      "eshell")))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-package-update cmake-mode zenburn-theme yasnippet-snippets yasnippet-classic-snippets xref-js2 which-key web-mode virtualenvwrapper virtualenv use-package undo-tree sphinx-doc spaceline sml-mode smartparens slime rainbow-delimiters pythonic pytest py-autopep8 projectile pdf-tools panda-theme org-pomodoro org nov multiple-cursors magit lsp-mode jupyter js-doc iter2 iedit htmlize helm-xref helm-spotify-plus helm-rg helm-flyspell helm-bind-key haskell-mode gruber-darker-theme goto-last-change go-snippets flymake-jslint flymake-grammarly flymake-go flymake-eslint flymake-diagnostic-at-point flycheck-pyflakes expand-region esup ess eslint-fix equake elpy django-snippets diminish delight default-font-presets dash-functional crux company-ycmd company-jedi company-go company-box company-auctex benchmark-init auto-virtualenv auto-dictionary auto-complete auctex-latexmk ahungry-theme ace-window ac-js2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
