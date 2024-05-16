(use-package org-journal
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  ;; (setq org-journal-prefix-key "C-c j ")
  (setq org-journal-dir my-journal-dir)
  :config

  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%Y/month%m.org" ;;"%Y%m%d.org" "%Y/month%m-week%V.org"
        org-journal-date-format "%b %e %Y (%A)"
        org-journal-time-format ""
        org-journal-start-on-weekday '7)

  (defun my/org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       ;; (`daily "#+AUTHOR: Haoming Shen\n#+OPTIONS: author:nil date:nil title:nil toc:nil broken-links:t\n#+LaTeX_CLASS: notes")
       ;; (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`weekly "#+AUTHOR: Haoming Shen\n#+OPTIONS: author:nil date:nil title:nil toc:nil broken-links:t\n#+STARTUP: overview\n#+LaTeX_CLASS: notes\nWeekly Goals [%]\nRESEARCH:\n- [ ] \nCOURSES:\n- [ ] \nSERVICE:\n- [ ] \nOTHERS:\n- [ ] \n\n")
       (`monthly "#+AUTHOR: Haoming Shen\n#+OPTIONS: author:nil date:nil title:nil toc:nil broken-links:t\n#+STARTUP: overview\n#+LaTeX_CLASS: notes")
       ;; (`yearly "#+AUTHOR: Haoming Shen\n#+OPTIONS: author:nil date:nil title:nil toc:nil broken-links:t\n#+LaTeX_CLASS: notes")
       )))

  (setq org-journal-file-header 'my/org-journal-file-header-func)

  ;; (defun get-journal-file-today ()
  ;;   "Gets filename for today's journal entry."
  ;;   (let ((daily-name (format-time-string "%Y%m")))
  ;;     (expand-file-name (concat org-journal-dir daily-name ".org"))))

  ;; (defun journal-file-today ()
  ;;   "Creates and load a journal file based on today's date."
  ;;   (interactive)
  ;;   (find-file (get-journal-file-today)))

  (defun journal-file-today ()
    "Creates and load a journal file based on today's date."
    (interactive)
    (org-journal-open-current-journal-file))
)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun my-org-mode-hooks ()
    (visual-line-mode)
    (display-line-numbers-mode t)
    (flyspell-mode)
    (outline-minor-mode)
    (electric-pair-mode 0)
    ;; (electric-pair-mode) ;; has to be disabled other wise \( ... \) cannot be paired properly
    )
  :general
  (defun my/open-agenda (&optional arg)
    "Open org-agenda directly"
    (interactive "p")
    (org-agenda arg "a"))
  (defun my/open-diary ()
    "Open org-diary directly"
    (interactive) ;; (find-file org-my-diary)
    (journal-file-today))
  (defun my/open-inbox ()
    "Open inbox directly"
    (interactive)
    (find-file my-org-inbox))
  (defun my/open-gtd ()
    "Open org-my-gtd directly"
    (interactive)
    (find-file my-org-gtd))
  (defun my/open-projects ()
    "Open org-research directly"
    (interactive)
    (find-file my-org-projects))
  (defun my/open-readings ()
    "Open org-readings directly"
    (interactive)
    (find-file my-org-reading))
  
  (leader
    "oa"  'my/open-agenda
    "od"  'my/open-diary
    "oi"  'my/open-inbox
    "od"  'my/open-gtd
    "op"  'my/open-projects
    "or"  'my/open-readings
    "ot"  'org-todo-list)
  :general-config
  (general-def org-mode-map
    "C-0" (lambda () (interactive) (org-latex-export-to-pdf t))
    "C-9" (lambda () (interactive) (org-beamer-export-to-pdf t))
    "C-<f9>" 'org-toggle-pretty-entities
    "C-<f10>" 'org-latex-preview)
  (general-def org-agenda-mode-map
    "<f10>" 'my/copy-idlink-to-clipboard)
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "FAILED(f)")
          (sequence "[.](T)" "[-](p)" "[?](m)" "|" "[X](D)")
          (sequence "NEXT(n)" "INPROGRESS(I)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)" "FAILED(f)")))
  
  ;; extend today for late sleepers
  ;; (setq org-extend-today-until 2)
  ;; Add time stamp and note to the task when it's done
  (setq org-log-done 'time)
  ;; Insert state change notes and time stamps into a drawer
  (setq org-log-into-drawer t)
  ;; use user preferred labels
  (setq org-latex-prefer-user-labels t)
  ;; Downscale image size
  ;; Source: https://emacs.stackexchange.com/questions/26363/downscaling-inline-images-in-org-mode
  (setq org-image-actual-width nil)
  ;; Add the REPORT drawer
  (setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "REPORT"))
  
  ;; Start week on Sunday (not following the ISO standard)
  (setq org-agenda-start-on-weekday 0)
  
  ;; control where the todo popup appears
  ;; source: https://emacs.stackexchange.com/questions/14817/how-to-control-where-the-org-todo-keywords-buffer-displays/17133#17133
  (setq org-use-fast-todo-selection 'expert)
  
  
  ;; https://stackoverflow.com/questions/17239273/org-mode-buffer-latex-syntax-highlighting
  (setq org-highlight-latex-and-related '(latex script entities))
  
  ;; https://emacs.stackexchange.com/questions/50667/org-mode-auto-fill-mode
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  
  ;; https://stackoverflow.com/questions/11365739/how-to-cancel-the-hypersetup-in-0rg-mode-of-emacs
  (setq org-latex-with-hyperref nil)
  
  (setq org-emphasis-alist
        '(("*" (bold :foreground "Orange" ))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:background "deep sky blue" :foreground "MidnightBlue"))))
  
  (add-to-list 'org-modules 'org-tempo t)
  
  ;; for ledger integration into orgmode
  (add-to-list 'org-babel-load-languages '(ledger . t))
  
  (setq org-structure-template-alist
    '(("lem" . "lemma")
      ("thm" . "theorem")
      ("cor" . "corollary")
      ("rmk" . "remark")
      ("prf" . "proof")
      ("prop" . "proposition")
      ("prob" . "problem")
      ("clm" . "claim")
      ("sol" . "solution")
      ("def" . "definition")
      ("emp" . "example")
      ("ltx" . "export latex")
      ("ledger" . "src ledger :noweb yes")
      ("el" . "src emacs-lisp")
      ("sh" . "src sh")
      ("src" . "src")
      ("exp" . "export")))
  
  (define-skeleton org-latex-header
    "Header info for literature notes."
    "Inserting header for literature notes."
    "#+DATE: \n"
    "#+AUTHOR: Haoming Shen\n"
    "#+OPTIONS: author:nil date:nil title:nil toc:nil \n"
    "#+LaTeX_CLASS: notes \n"
    "#+LaTeX_HEADER: \\addbibresource{master.bib} \n"
   )
  
  (define-skeleton org-header
    "Header info for org notes."
    "Inserting header for org notes."
    "#+DATE: \n"
    "#+AUTHOR: Haoming Shen\n"
   )
  
  (define-skeleton org-latex-attr
    "Attributes for LaTeX segments"
    "Inserting attributes for LaTeX environment."
    "#+ATTR_LaTeX: :options []"
    )
  (defun my/copy-idlink-to-clipboard()
     "Copy an ID link with the headline to killring, if no ID is there then create a new unique ID. This function works only in org-mode or org-agenda buffers. The purpose of this function is to easily construct id:-links to org-mode items. If its assigned to a key it saves you marking the text and copying to the killring."
        (interactive)
        (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
      (org-agenda-show)
      (org-agenda-goto))
        (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
      (setq mytmphead (nth 4 (org-heading-components)))
          (setq mytmpid (funcall 'org-id-get-create))
      (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
      (kill-new mytmplink)
      (message "Copied %s to killring (clipboard)" mytmplink)))
  
  (defun my/tex-canvasify ()
    (interactive)
    (save-excursion
      (replace-regexp "\\$\\(.*?\\)\\$" "\\\\(\\1\\\\)" nil (point-min) (point-max))))
  
  (defun my/tex-uncanvasify ()
      (interactive)
      (save-excursion
        (replace-regexp "\\\\\(.*?\)\\\\)" "\\$\\\1\\\$" nil (point-min) (point-max))))
  
  (setq org-capture-bookmark nil)
  
  (defun my/org-journal-find-location ()
     ;; Open today's journal, but specify a non-nil prefix argument in order to
     ;; inhibit inserting the heading; org-capture will insert the heading.
     (org-journal-new-date-entry t)
     (unless (eq org-journal-file-type 'daily)
       (org-narrow-to-subtree))
     (goto-char (point-max)))
  
  (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline org-my-inbox "Tasks") "* TODO %i")
            ;; ("T" "Tickler" entry
            ;;  (file+headline "~/Dropbox/Org/tickler.org" "Tickler") "* %i%? \n %U")
            ("d" "Daily Tasks in Journal" plain (function my/org-journal-find-location)
             "** Tasks [%]\nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ] Org my life. \n- [ ] Anki my Memory. \n- [ ] Enjoy my day. \n- [ ] Keep Exercising."
             :immediate-finish t
             :jump-to-captured t)
            ("w" "Weekly Tasks in Journal" plain (function my/org-journal-find-location)
             "* Weekly Goals [%]\nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ]"
             :immediate-finish t
             :jump-to-captured t
             )
            ;; ("d" "Daily Tasks in Journal" entry (file+datetree+prompt org-my-diary)
            ;;  "** Tasks [%]\nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ] Org my life. \n- [ ] Enjoy my day. \n- [ ] Keep Exercising."
            ;;  :tree-type week
            ;;  :immediate-finish t
            ;;  :jump-to-captured t
            ;;  )
             ("l" "Ledger entries")
            ("lC" "Chase CSP" plain
                     (file my-ledger)
                     "%(org-read-date) * %^{Payee}
      Expenses:%^{Category}:%^{Details}  %^{Amount}
      Liabilities:Chase:SapphirePreferred
    ")
            ("lF" "Chase Freedom" plain
                     (file my-ledger)
                     "%(org-read-date) * %^{Payee}
      Expenses:%^{Category}:%^{Details}  %^{Amount}
      Liabilities:Chase:FreedomUnlimited
    ")
            ("lB" "Amex BlueCash" plain
                     (file my-ledger)
                     "%(org-read-date) * %^{Payee}
      Expenses:%^{Category}:%^{Details}  %^{Amount}
      Liabilities:Amex:BlueCash
    ")
            ("A" "Anki entries")))
  
  ;; refile configs
  (defun my/opened-buffer-files ()
      "Return the list of files currently opened in emacs"
      (delq nil
  	(mapcar (lambda (x)
  		(if (and (buffer-file-name x)
  			    (string-match "\\.org$"
  					(buffer-file-name x)))
  		    (buffer-file-name x)))
  		(buffer-list))))
  
  (setq org-refile-targets '((my-org-gtd :maxlevel . 3)
  			(my-org-projects :maxlevel . 3)
  			(my-org-reading :maxlevel . 3)
  			(my-org-proposals :maxlevel . 3)
  			(my-org-teaching :maxlevel . 3)
  			(my-org-misc :maxlevel . 3)
  			(my/opened-buffer-files :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps t)
  (setq org-directory (list my-org-dir))
  (setq org-agenda-files
        (list
         my-org-inbox
         my-org-gtd
         my-org-misc
         my-org-projects
         my-org-teaching
         my-org-reading
         my-org-proposals))
  
  (setq org-archive-location my-org-archive)
  )

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "Projects"
                :deadline today
                :priority "A")
         (:name "Overdue"
                :deadline past)
         (:name "Due soon"
                :deadline future)
         (:name "To read"
                :tag "Papers")
         (:name "Personal"
                :habit t)
         (:name "Less Important"
                :priority<= "B"
                :order 7)
         (:todo ("WAITING" "LATER")
                :order 8)
         (:name "Not Urgent"
                :todo "TODO"
                :order 9)))
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator t
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t)
  :config
  (org-super-agenda-mode))

(provide 'my-org)
