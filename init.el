; init.el --- An Emacs configuration file.
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; ;; TODO introduce evil package

;;; Code:
;;  configures main functionality and loads external config files

;; Select the folder to store packages and add package repositories
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;TODO: What does this do
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package auto-package-update
  ;; Keeps the packages updated
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; In days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; Add folders for configuration files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))

;; ;TODO: Figure out how to load onlyif file exists.
(load-library "org_config")
(load-library "python_config")
(load-library "web_dev")
(load-library "latex_config")
(load-library "personal_information")
(load-library "custom-set")

;; Color theme
(use-package gruber-darker-theme :config (load-theme 'gruber-darker t))

(setq-default inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      visible-bell nil
      indent-tabs-mode -1
      tab-width 4
      ;; No auto backup of files to avoid clutter
	  make-backup-files nil
	  indent-tabs-mode nil
	  show-trailing-whitespace t
      ;; The week starts on Monday
      calendar-week-start-day 1
      ;; Collect custom options in a separate file to avoid clutter
      custom-file "~/.emacs.d/personal/custom-set.el")

(defalias 'yes-or-no-p 'y-or-n-p)

;; Tool bar waste space.
(tool-bar-mode -1)
;; Indent when pressing RET.
(global-set-key (kbd "RET") 'newline-and-indent)
;; Trailing whitespace are never welcome.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Wrap text if text moves outside frame width.
(global-visual-line-mode 1)

;; Highlight current line in active window.
(global-hl-line-mode 1)

;; Always show line numbers when programming.
(add-hook 'prog-mode-hook '(linum-mode -1))

(use-package crux
  ;; Provides shortcuts for common tasks
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-k" . crux-smart-kill-line)
   ("C-c u" . crux-view-url)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("M-o" . crux-other-window-or-switch-buffer)
   ("C-c I" . crux-find-user-init-file)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; TODO use in for quick region selection latex python
(use-package expand-region
  ;; quickly mark/unmark regions of text
  :bind
  (("C-," . er/expand-region)
   ("C-M-," . er/contract-region)))

(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode org-mode) . company-mode)
  :commands (company-complete-common)
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (global-company-mode 1)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-echo-delay 0))

(use-package smartparens
  ;; package for smart handling and navigation of delimiters ("(","[", etc.)
  :hook (prog-mode . smartparens-mode)
  :commands (sp-local-pair)
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-M-e" . sp-up-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-k" . sp-change-enclosing)
        ("M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil))

(use-package pdf-tools-install
  :defer t
  :ensure pdf-tools
  :no-require t
  :mode "\\.pdf\\'"
  :commands (pdf-loader-install)
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  ;; (linum-mode -1) ;; Line numbers doesn't make sense for PDFs.
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  :bind
  (("C-s" . isearch-forward)
   ("C-r" . isearch-backward)))

(use-package flycheck
  :defer t
  :diminish
  :hook (after-init . global-flycheck-mode)
  :commands (flycheck-add-mode)
  :custom
  (flycheck-global-modes
   '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin))
  :init
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face
        (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-position 'window-bottom-left-corner)
        (flycheck-posframe-border-width 3)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :config
  (with-eval-after-load 'undo-tree
    (setq undo-tree-auto-save-history nil)))

(use-package magit
  :bind
  (("C-c g" . magit-status)
   ("C-c j" . magit-dispatch)
   ("C-c k" . magit-file-dispatch)
   ("C-c z" . magit-log-buffer-file)
   ("C-c b" . magit-blame)))

;; TODO what does this do
(use-package delight )

;; TODO configure helm
(use-package helm
  :delight
  :commands (helm-autoresize-mode)
  :bind
  (("M-x"     . #'helm-M-x))
  (("C-x C-f" . #'helm-find-files))
  (("C-x C-b" . #'helm-buffers-list))
  (("M-y" .     #'helm-show-kill-ring))
  (("C-x b" .   #'helm-mini))
  :init
  (helm-mode t)
  :config
  (use-package helm-flyspell :after (helm flyspell))
  (use-package helm-xref )
  (use-package helm-rg )
  (helm-autoresize-mode 1)
  (setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-use-undecorated-frame-option t))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package spaceline
  :config
  (use-package spaceline-config
    :ensure spaceline
    :config
    (spaceline-toggle-minor-modes-off)
    (spaceline-helm-mode 1)
    (spaceline-emacs-theme)
    (format-time-string "%H:%M")
    (spaceline-define-segment date
                              "The current date."
                              (format-time-string "%h %d"))
    ;(spaceline-toggle-time-on)
    (spaceline-emacs-theme 'date 'time))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package which-key
  ;; Possible keyboard shortcuts popup window
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.33))

(use-package helm-spotify-plus
  ;; Control Spotify from Emacs
  :bind
  (("C-c s s" . helm-spotify-plus)
   ("C-c s f" . helm-spotify-plus-next)
   ("C-c s b" . helm-spotify-plus-previous)
   ("C-c s p" . helm-spotify-plus-play)
   ("C-c s g" . helm-spotify-plus-pause)))

(use-package equake
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

(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save))
  ;;:custom (gofmt-command "goimports"))

(provide 'init)
;;; init.el ends here
