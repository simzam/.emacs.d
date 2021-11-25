;;; python_config.el --- Configuration of Python
;; Author:  simzam
;; Keywords: config, emacs, python
;;; Commentary:

;;

;;; Code:
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; (use-package elpy
;;   :ensure t
;;   :delight
;;   :mode
;;   ("\\.py\\'" . python-mode)
;;   ("\\.wsgi$" . python-mode)
;;   :init
;;   ;; (defun company-jedi-setup ()
;;   ;;   (add-to-list 'company-backends 'company-jedi))
;;   ;; (add-hook 'python-mode-hook 'company-jedi-setup)
;;   (defvar py-python-command "python3")
;;   (elpy-enable)

;;   (use-package py-autopep8
;;     :ensure t)
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;   :config
;;   ;; (defvar jedi:setup-keys t)
;;   ;; (add-hook 'python-mode-hook 'jedi:setup)
;;   ;; (defvar elpy-rpc-backend "jedi")
;;   ;; (defvar jedi:complete-on-dot t)
;;   (setq python-indent-offset 4)
;;   (setq python-indent-guess-indent-offset-verbose nil)

;;   (with-elpy-rpc-virtualenv-activated
;;    (message "RPC binaries: '%s'" (executable-find elpy-rpc-python-command)))
;;   (message "User binaries: '%s'" (executable-find elpy-rpc-python-command))
;; )
;; Python mode
;;; Python

;; virtualenvwrapper.el
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (setq venv-location "~/.virtualenvs/")
  (setq-default mode-line-format
                (append mode-line-format '((:exec venv-current-name)))))


;; Autocomplete/Jedi Setup
(use-package jedi
  :ensure t

  :init
  (setq jedi:setup-keys t)
  (setq jedi:use-shortcuts t)
  (setq jedi:complete-on-dot t)
  ;; show function signatures in mini-buffer instead of popup
  (setq jedi:tooltip-method nil)

  :hook ((python-mode . jedi:setup)
         (inferior-python-mode . jedi:setup)))


(use-package pytest
  :ensure t
  :bind ("C-c C-y" . pytest-module))


;; Set ipython as the python interpreter
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--colors NoColor --simple-prompt"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; Electric pairs
(add-hook 'python-mode-hook 'electric-pair-mode)

;; Smartparens for python
(add-hook 'python-mode-hook 'smartparens-mode)

;; Flycheck
(add-hook 'python-mode-hook 'flycheck-mode)


;; Enable rainbow-delimiters in python-mode
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)


;; sphinx-doc
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; (defun my-merge-imenu ()
;;   (interactive)
;;   (let ((mode-imenu (imenu-default-create-index-function))
;;         (custom-imenu (imenu--generic-function imenu-generic-expression)))
;;     (append mode-imenu custom-imenu)))

;; (defun my-python-hooks()
;;     (interactive)
;;     (setq tab-width     4
;;           python-indent 4
;;           python-shell-interpreter "ipython"
;;           python-shell-interpreter-args "-i")
;;     (if (string-match-p "rita" (or (buffer-file-name) ""))
;;         (setq indent-tabs-mode t)
;;       (setq indent-tabs-mode nil)
;;     )
;;     (add-to-list
;;         'imenu-generic-expression
;;         '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
;;     (setq imenu-create-index-function 'my-merge-imenu)
;;     ;; pythom mode keybindings
;;     (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
;;     (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
;;     (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
;;     (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names)
;;     ;; end python mode keybindings

;;     (eval-after-load "company"
;;         '(progn
;;             (unless (member 'company-jedi (car company-backends))
;;                 (setq comp-back (car company-backends))
;;                 (push 'company-jedi comp-back)
;;                 (setq company-backends (list comp-back)))
;;             )))

;; (add-hook 'python-mode-hook 'my-python-hooks)
;; ;; End Python mode

(provide 'python_config)
;;; python_config.el ends here
