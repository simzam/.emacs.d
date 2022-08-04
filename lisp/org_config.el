;;; .org_config.el --- Configuration file for org mode
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; org mode configuration requires many lines are easier handled in a
;; separate file

;;; Code:
(use-package org
  :ensure t
  :delight
  :init
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
  ;; with 0:00 duration
  (defvar org-clock-out-remove-zero-time-clocks t)

  ;; clock out when moving task to a done state
  (defvar  org-clock-out-when-done t)

  ;; depth in hierarchy to look for refile headlines
  (defvar org-refile-targets
    '((nil :maxlevel . 3)
      (org-agenda-files :maxlevel . 3)))

  (setq org-support-shift-select t)
  (add-hook 'org-mode-hook 'org-indent-mode)

  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-directory "~/Documents/.org/")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("t" "week me!" entry (file "~/Documents/.org/week.org")
	   "* TODO %?\n %i\n %a")
	  ("j" "journal" entry (file+olp+datetree ".p.org")
	   "* %? %^G \n\n%U\n" :kill-buffer 1)
	  ))

  ;; increase size of embedded latex compiled written math
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-src-fontify-natively t)

  (setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))


  (use-package org-pomodoro
    :ensure t
    :config
    (setq org-pomodoro-play-sounds nil))

  ;;; add the tag :crypt: too entries for encryption
  (use-package org-crypt
    :init
    (setq org-tags-exclude-from-inheritance '(quote ("crypt")))
    ;; org-crypt-key nil means symmetric encryption
    (setq org-crypt-key nil)
    (setq auto-save-default nil)
    :config
    (org-crypt-use-before-save-magic))

  )
(provide 'org_config)
;;; org_config ends here
