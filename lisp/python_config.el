;;; python_config.el --- Configuration of Python
;; Author:  simzam
;; Keywords: config, emacs, python
;;; Commentary:

;;

;;; Code:

;; clean up code according to PEP8
(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(defun my/python-mode-hook ()
  "Since company does autocomplete for us we need the function below."
  (add-to-list 'company-backends 'company-jedi))

(use-package elpy
  :ensure t
  :delight python-mode
  :mode
  ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :init
  ;; loading Jedi for company through function.
  (with-eval-after-load 'company 'my/python-mode-hook)

  ;; enable elpy for all Python files
  (with-eval-after-load 'python (elpy-enable))

  :config
  ;; setting indent
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; coloring layers of parenthesis for readability
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
)

;; automatically sets the virtual environment based on the name of the
;; project folder. Virtual environments are to be found under
;; "~/.virtualenvs" and all Python code must be in a project with a
;; separate virtual environment. Elpy relies on virtual environment to
;; function properly.
(use-package auto-virtualenv
  :ensure t
  :diminish t
  :config
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'python-mode 'auto-virtualenv-set-virtualenv)
  )

;; move cursor to Python function and press "C-c M-d"!
(use-package sphinx-doc
  :ensure t
  :diminish t
  :config
  (add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))
  )

;; (use-package pytest
;;   :ensure t
;;   :bind
;;   (("C-c C-y" . pytest-module)))

(provide 'python_config)
;;; python_config.el ends here
