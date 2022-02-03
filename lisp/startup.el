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

(use-package benchmark-init
  :ensure t
  :init
  ;; increase size of garbage collection during startup
  (setq gc-cons-threshold 10000000)

  ;; restore after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 1000000)
              (message "gc-cons-threshold restored to %S"
                       gc-cons-threshold)))
  :config
  ;; to disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

  ;; restore after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 1000000)
              (message "gc-cons-threshold restored to %S"
                       gc-cons-threshold)))
(provide 'startup)
;;; startup ends here
