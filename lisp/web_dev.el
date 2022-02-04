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
(provide 'web_dev)
;;; web_dev.el ends here
