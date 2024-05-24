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

  (defun my/open-diary ()
    (interactive)
    "Open org-diary directly"
    (journal-file-today))

  (leader
    "nd" 'my/open-diary)
)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun my-org-mode-hooks ()
    (visual-line-mode)
    (outline-minor-mode)
    (electric-pair-mode -1)) ;; electric-pair-mode has to be disabled other wise \( ... \) cannot be paired properly
  ;; (my-org-mode-hooks)
  :hook (org-mode . my-org-mode-hooks)
  :general
  (defun my/open-agenda (&optional arg)
    "Open org-agenda directly"
    (interactive "p")
    (org-agenda arg "a"))
  
  (leader
    "na"  'my/open-agenda
  
    "X"   'org-capture
    )
  
  :general-config
  (general-def org-mode-map
    "C-0" (lambda () (interactive) (org-latex-export-to-pdf t))
    "C-9" (lambda () (interactive) (org-beamer-export-to-pdf t))
    "C-<f9>" 'org-toggle-pretty-entities
    "C-<f10>" 'org-latex-preview)
  
  (general-def '(org-mode-map org-agenda-mode-map)
    "<f10>" 'my/copy-idlink-to-clipboard)
  
  (general-def
    :states 'motion
    :keymaps '(org-mode-map org-agenda-mode-map)
    "RET" 'org-return
    )
  
  (local-leader
    :keymaps 'org-mode-map
    "t"  '(:ignore t :which-key "org-entry")
    "ta" 'org-archive-subtree
    "tP" 'org-set-property
    "tp" 'org-priority
    "tt" 'org-todo
  
    "r"  '(:ignore t :which-key "org-refile")
    "rr" 'org-refile
    "rc" 'org-refile-copy
    )
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
  
  (setq org-return-follows-link  t)
  
  ;; Start week on Sunday (not following the ISO standard)
  (setq org-agenda-start-on-weekday 7)
  
  ;; use mm-dd-yyyy
  (setq org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>"))
  (setq org-display-custom-times t)
  
  ;; control where the todo popup appears
  ;; source: https://emacs.stackexchange.com/questions/14817/how-to-control-where-the-org-todo-keywords-buffer-displays/17133#17133
  (setq org-use-fast-todo-selection 'expert)
  
  
  ;; https://stackoverflow.com/questions/17239273/org-mode-buffer-latex-syntax-highlighting
  (setq org-highlight-latex-and-related '(latex script entities))
  
  ;; https://emacs.stackexchange.com/questions/50667/org-mode-auto-fill-mode
  ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
  
  ;; https://stackoverflow.com/questions/11365739/how-to-cancel-the-hypersetup-in-0rg-mode-of-emacs
  (setq org-latex-with-hyperref nil)
  
  (setq org-emphasis-alist
        '(("*" (bold :foreground "Blue" ))
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
  
  ;; init an empty list
  (setq org-capture-templates nil)
  ;; push values into it
  (add-to-list 'org-capture-templates
  	     '("t" "Todo [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* TODO %i"))
  
  (add-to-list 'org-capture-templates
  	     '("d" "Daily Tasks in Journal" plain (function my/org-journal-find-location)
  	       "** Tasks [%]\nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ] Org my life. \n- [ ] Enjoy my day. \n- [ ] Keep Exercising."
  	       :immediate-finish t
  	       :jump-to-captured t))
  
  (add-to-list 'org-capture-templates
  	     '("w" "Weekly Tasks in Journal" plain (function my/org-journal-find-location)
  	       "* Weekly Goals [%]\nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ]"
  	       :immediate-finish t
  	       :jump-to-captured t))
  
  (add-to-list 'org-capture-templates
  	     '("p" "Proposal to write [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* [%^{SHORT}] %^{PROPOSAL TITLE} [%]
    :PROPERTIES:
    :COOKIE_DATA: todo recursive
    :END:\n** TODO [%\\1] Prep. the budget form\n** TODO [%\\1] Literature Review [%]\n** TODO [%\\1] Proposal Writing [%]\n** TODO [%\\1] Supplementary Doc Prep. [%]"))
  
  (add-to-list 'org-capture-templates
  	     '("r" "Research project to think [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* [%^{SHORT}] %^{PROJECT TITLE} [%]
    :PROPERTIES:
    :COOKIE_DATA: todo recursive
    :END:\n** TODO [%\\1] Literature review [%]\n** TODO [%\\1] Research questions [%]\n** TODO [%\\1] Paper writing [%]"))
  
  (add-to-list 'org-capture-templates
  	     '("R" "Paper/Proposal to review [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* [%^{SHORT}] %^{TITLE} [%]
    :PROPERTIES:
    :COOKIE_DATA: todo recursive
    :END:\n** TODO [%\\1] Submission overview\n** TODO [%\\1] Submission evaluation[%]\n** TODO [%\\1] Review letter writing [%]"))
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

(use-package org-roam
  :custom
  (org-roam-directory (file-truename my-org-roam-dir))
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-node-display-template
	(concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target (file+head "%<%Y%m%d%H%M>-${slug}.org"
                          "#+TITLE: ${title}\n#+DATE: \n#+AUTHOR: Haoming Shen \n#+OPTIONS: author:nil date:nil title:nil toc:nil\n#+LaTeX_CLASS: notes\n#+LaTeX_HEADER: \\addbibresource{master.bib}")
       :unnarrowed t)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)

  :general

  (leader
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert
    ))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} - ${title}")
  (add-to-list 'org-roam-capture-templates
               '("n" "literature note" plain "%?"
                 :target
                 (file+head
                  "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
                  "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
                 :unnarrowed t)))

(provide 'my-org)
