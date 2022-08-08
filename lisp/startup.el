;;; startup.el --- Startup script
;;; loading of modules
;; Author:  simzam
;; Keywords: config, emacs, startup, benchmark
;;; Commentary:

;; Loading package module and module for benchmarking, Simplifying
;; debugging slow loading times.

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives ())
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package esup
  :ensure t
  :pin melpa
  :init
  (setq esup-depth 0))

(provide 'startup)
;;; startup ends here
