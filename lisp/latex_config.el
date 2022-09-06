2;;; latex_config.el --- Configuration for latex
;; Author: simzam
;; Keywords: config, emacs, latex, tex, pdf
;;; Commentary:

;; Configuration of latex.

;;; Code:
;; (use-package tex
;;   :ensure auctex
;;   :init
;;   (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
;;   :config
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;     TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
;;   (add-hook 'TeX-after-compilation-finished-functions
;;         #'TeX-revert-document-buffer))

(use-package auto-dictionary
  :ensure t
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package company-auctex
  :ensure t)

(use-package tex
  :commands (TeX-revert-document-buffer)
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
	    (setq TeX-source-correlate-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    ;;(setq-default TeX-master "paper.tex")
	    (setq reftex-plug-into-AUCTeX t)
	    (pdf-tools-install)
	    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t)
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
	    (add-hook 'LaTeX-mode-hook
		      (lambda ()
			(reftex-mode t)
			(flyspell-mode t)))))

(provide 'latex_config)
;;; latex_config.el ends here
