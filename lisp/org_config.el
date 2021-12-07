;;; .org_config.el --- Configuration file for org mode
;; Author:  simzam
;; Keywords: config, emacs
;;; Commentary:

;; org mode configuration requires many lines are easier handled in a
;; separate file

;;; Code:
(use-package org
  :delight
  :init
  ;;(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
  ;; with 0:00 duration
  (defvar org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (defvar  org-clock-out-when-done t)

  ;; depth in hierarchy to look for refile headlines
  (defvar org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (setq org-support-shift-select t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "ISSUE(p)" "INPUTNEEDED(i)"
        | "SCOPECHANGE(r)" "DONE(d)" "CANCELLED(c)")))

  (setq org-tag-alist
        '(("fix" . ?f) ("message" . ?m) ("buy" . ?b) ("read" . ?r)))

  ;; (add-hook 'org-mode-hook 'org-indent-mode)

  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  ;; (global-set-key (kbd "C-c c") 'org-capture)

  ;; (defvar org-capture-templates
  ;;     '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
  ;;        "* TODO %?\n  %i\n  %a")
  ;;       ("j" "Journal" entry (file+datetree "~/org/journal.org")
  ;;        "* %?\nEntered on %U\n  %i\n  %a")))

  ;; (setq org-capture-templates
  ;;     '(("t" "TODO" entry (file "week.org")
  ;; 	   "* eTODO %? %^G \n  %U" :empty-lines 1)
  ;; 	  ("s" "Scheduled TODO" entry (file "week.org")
  ;; 	   "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
  ;; 	  ("d" "Deadline" entry (file "week.org")
  ;; 	   "* TODO %? %^G \n DEADLINE: %^t" :empty-lines 1)
  ;; 	  ("p" "Priority" entry (file "week.org")
  ;; 	   "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
  ;; 	  ("j" "Journal" entry (file+datetree ".p.org")
  ;; 	   "* %? %^G\nEntered on %U\n" :empty-lines 1))
  ;;     )

  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

  ;; (defvar org-pomodoro-play-sounds nil)

  ;; ;;; TODO read up on package
  ;; (use-package org-crypt
  ;;   :init
  ;;   (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;;   (setq org-crypt-key nil)
  ;;   :config
  ;;   (org-crypt-use-before-save-magic))

  ;; ;; TODO read up on package
  ;; (use-package epa-file
  ;;   :init
  ;;   (epa-file-enable)
  ;;   )
  )
(provide 'org_config)
;;; org_config ends here
