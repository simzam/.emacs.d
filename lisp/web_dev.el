;;; web_dev.el --- Configuration of web development tools
;; Author: simzam Keywords: config, emacs, web, development, django,
;; css, html
;;; Commentary:

;; Assorted functionality used for web development.

;;; Code:
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  :config
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "dark orange")
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :foreground "black"
                      :background "gold")
  (set-face-attribute 'web-mode-current-column-highlight-face nil
                      :background "gold")
  )

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  :config
  (define-key js-mode-map (kbd "M-.") nil)
)

(use-package xref-js2
    :ensure t
    :init
    (add-hook 'js2-mode-hook (lambda ()
                               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
)

;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((js-mode         ; ts-ls (tsserver wrapper)
;;           js-jsx-mode     ; ts-ls (tsserver wrapper)
;;           typescript-mode ; ts-ls (tsserver wrapper)
;;           ) . lsp-deferred)
;;   :commands lsp
;;   :config
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-log-io nil)
;;   (setq lsp-restart 'auto-restart)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-enable-on-type-formatting nil)
;;   (setq lsp-signature-auto-activate nil)
;;   (setq lsp-signature-render-documentation nil)
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-modeline-code-actions-enable nil)
;;   (setq lsp-modeline-diagnostics-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-semantic-tokens-enable nil)
;;   (setq lsp-enable-folding nil)
;;   (setq lsp-enable-imenu nil)
;;   (setq lsp-enable-snippet nil)
;;   (setq read-process-output-max (* 1024 1024)) ;; 1MB
;;   (setq lsp-idle-delay 0.5))

(provide 'web_dev)
;;; web_dev.el ends here
