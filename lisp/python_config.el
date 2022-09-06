;;; python_config.el --- Configuration of Python
;; Author:  simzam
;; Keywords: config, emacs, python
;;; Commentary:

;;

;;; Code:
;; (use-package py-autopep8
;;   :ensure t
;;   :init
;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;; (use-package auto-virtualenv
;;   :ensure t
;;   :diminish t
;;   :config
;;   (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
;;   (add-hook 'python-mode 'auto-virtualenv-set-virtualenv))

;; (use-package sphinx-doc
;;   :ensure t
;;   :diminish t
;;   :config
;;   (add-hook 'python-mode-hook (lambda ()
;;                               (require 'sphinx-doc)
;;                               (sphinx-doc-mode t))))

;; (use-package lsp-mode
;;   :demand t
;;   :config
;;   (defun md/lsp-setup()
;;     ;; recommended by LSP docs for performance
;;     (setq read-process-output-max (* 1024 1024)) ;; 1mb

;;     (lsp-enable-imenu)
;;     (setq
;;           lsp-auto-configure t
;;           lsp-enable-dap-auto-configure nil ; Don't try to auto-enable dap: this creates a lot of binding clashes
;;           lsp-auto-guess-root t ; Uses projectile to guess the project root.
;;           lsp-before-save-edits t
;;           lsp-eldoc-enable-hover t
;;           lsp-eldoc-render-all nil
;;           lsp-completion-enable t
;;           lsp-completion-show-detail t
;;           lsp-completion-show-kind t
;;           lsp-enable-file-watchers t
;;           lsp-file-watch-threshold 100
;;           lsp-enable-folding t
;;           lsp-enable-imenu t
;;           lsp-enable-indentation t
;;           lsp-enable-links t
;;           lsp-clients-python-library-directories `("/usr/" ,(expand-file-name "~/.virtualenvs")) ; This seems appropriate
;;           lsp-enable-on-type-formatting nil
;;           lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
;;           lsp-enable-symbol-highlighting nil
;;           lsp-enable-text-document-color nil
;;           lsp-enable-xref t
;;           lsp-flycheck-live-reporting nil
;;           lsp-idle-delay 0.5
;;           lsp-imenu-show-container-name t
;;           lsp-imenu-sort-methods '(position kind name)
;;           lsp-pyls-plugins-flake8-enabled t
;;           lsp-signature-auto-activate t
;;           lsp-signature-render-documentation t
;;           lsp-signature-doc-lines 10)
;;     (lsp-register-custom-settings
;;      '(("pyls.plugins.pyls_mypy.enabled" t t)
;;        ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;        ("pyls.plugins.pyls_black.enabled" t t)
;;        ("pyls.plugins.pyls_isort.enabled" t t)

;;        ;; Disable these as they're duplicated by flake8
;;        ("pyls.plugins.pycodestyle.enabled" nil t)
;;        ("pyls.plugins.mccabe.enabled" nil t)
;;        ("pyls.plugins.pyflakes.enabled" nil t))))
;;   :hook
;;    ;; NOTE: we don't have a python-mode hook - it gets handled by pyvenv-track-virtualenv
;;   (;;(js-mode . lsp)
;;    ;;(web-mode . lsp)
;;    (lsp-mode . lsp-enable-which-key-integration)
;;    (lsp-before-initialize . md/lsp-setup))
;;   ;; :bind (:map evil-normal-state-map
;;   ;;             ("gh" . lsp-describe-thing-at-point)
;;   ;;             ("gr" . lsp-find-references)
;;   ;;             ("gD" . xref-find-apropos)
;;   ;;             ("gd" . lsp-find-definition)
;;   ;;             :map md/leader-map
;;   ;;             ("Ni" . imenu)
;;   ;;             ("Ff" . lsp-format-buffer)
;;   ;;             ("FR" . lsp-rename)))
;;   )

;; (lsp-defun lsp-ui-doc--callback ((hover &as &Hover? :contents) bounds buffer)
;;   "Process the received documentation.
;; HOVER is the doc returned by the LS.
;; BOUNDS are points of the symbol that have been requested.
;; BUFFER is the buffer where the request has been made."
;;   (if
;;       (not (and
;;             hover
;;             (>= (point) (car bounds)) (<= (point) (cdr bounds))
;;             (eq buffer (current-buffer))))
;;       (setq contents "-")
;;     (setq contents (or (-some->>
;;                         ;; "shane"
;;                         contents
;;                         lsp-ui-doc--extract
;;                         (replace-regexp-in-string "\r" ""))
;;                        ;; (replace-regexp-in-string "\r" "" (lsp-ui-doc--extract contents))
;;                        "Cant extract or docs are empty")))

;;   (progn
;;     (setq lsp-ui-doc--bounds bounds)
;;     (lsp-ui-doc--display
;;      (thing-at-point 'symbol t)
;;      contents))
;;   ;; (lsp-ui-doc--hide-frame)
;;   )


;; (defun lsp-ui-doc--extract (contents)
;;   "Extract the documentation from CONTENTS.
;; CONTENTS can be differents type of values:
;; MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
;; We don't extract the string that `lps-line' is already displaying."
;;   ;; (tv contents)
;;   (cond
;;    ((vectorp contents) ;; MarkedString[]
;;     (mapconcat 'lsp-ui-doc--extract-marked-string
;;                (lsp-ui-doc--filter-marked-string (seq-filter #'identity contents))
;;                "\n\n"
;;                ;; (propertize "\n\n" 'face '(:height 0.4))
;;                ))
;;    ;; when we get markdown contents, render using emacs gfm-view-mode / markdown-mode
;;    ((and (lsp-marked-string? contents)
;;          (lsp:marked-string-language contents))
;;     (lsp-ui-doc--extract-marked-string (lsp:marked-string-value contents)
;;                                        (lsp:marked-string-language contents)))
;;    ((lsp-marked-string? contents) (lsp-ui-doc--extract-marked-string contents))
;;    ((and (lsp-markup-content? contents)
;;          (string= (lsp:markup-content-kind contents) lsp/markup-kind-markdown))
;;     (lsp-ui-doc--extract-marked-string (lsp:markup-content-value contents) lsp/markup-kind-markdown))
;;    ((and (lsp-markup-content? contents)
;;          (string= (lsp:markup-content-kind contents) lsp/markup-kind-plain-text))
;;     (lsp:markup-content-value contents))
;;    (t
;;     ;; This makes python work
;;     contents)))


;; (defun my-lsp-get-hover-docs ()
;;   (interactive)
;;   (let* ((ht (lsp-request "textDocument/hover" (lsp--text-document-position-params)))
;;          (docs
;;           (if (hash-table-p ht)
;;               (lsp-ui-doc--extract (gethash "contents" ht))
;;             "")))
;;     (if (called-interactively-p 'interactive)
;;         ;; (tvd docs)
;;         (new-buffer-from-string docs)
;;       docs)))


;; (use-package python-mode
;;   :ensure nil
;;   :after flycheck
;;   :mode "\\.py\\'"
;;   :hook (python-mode)
;;   :custom
;;   (python-indent-offset 4)
;;   (flycheck-python-pycompile-executable "python3")
;;   (python-shell-interpreter "python3"))

;; (use-package pip-requirements :demand t)

;; (use-package lsp-mode
;;   :ensure t
;;   :config

;;   ;; make sure we have lsp-imenu everywhere we have LSP
;;   (use-package lsp-ui-imenu)
;;   (add-hook 'lsp-eafter-open-hook 'lsp-enable-imenu)
;;   ;; get lsp-python-enable defined
;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
;;   ;;     or any other function that can be used to find the root directory of a project
;;   ;; (lsp-define-stdio-client lsp-python "python"
;;   ;;                          #'projectile-project-root
;;   ;;                          '("pyls"))

;;   ;; make sure this is activated when python-mode is activated
;;   ;; lsp-python-enable is created by macro above
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (lsp-python-enable)))

;;   ;; lsp extras
;;   (use-package lsp-ui
;;     :ensure t
;;     :config
;;     (setq lsp-ui-sideline-ignore-duplicate t)
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;   (use-package company-lsp
;;     :config
;;     (push 'company-lsp company-backends))

;;   ;; NB: only required if you prefer flake8 instead of the default
;;   ;; send pyls config via lsp-after-initialize-hook -- harmless for
;;   ;; other servers due to pyls key, but would prefer only sending this
;;   ;; when pyls gets initialised (:initialize function in
;;   ;; lsp-define-stdio-client is invoked too early (before server
;;   ;; start)) -- cpbotha
;;   (defun lsp-set-cfg ()
;;     (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
;;       ;; TODO: check lsp--cur-workspace here to decide per server / project
;;       (lsp--set-configuration lsp-cfg)))

;;   (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))


;; ;; (use-package pyvenv
;; ;;   :demand t
;; ;;   :config
;; ;;   (setq pyvenv-workon "emacs")  ; Default venv
;; ;;   (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

(provide 'python_config)
;;; python_config.el ends here
