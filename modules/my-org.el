(use-package ox-gfm)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun my-org-mode-hooks ()
    (visual-line-mode)
    (outline-minor-mode)
    (push '("[ ]" .  "☐") prettify-symbols-alist)
    (push '("[X]" . "☑" ) prettify-symbols-alist)
    (push '("[-]" . "❍" ) prettify-symbols-alist)
    (prettify-symbols-mode)
    (electric-pair-mode -1)) ;; electric-pair-mode has to be disabled other wise \( ... \) cannot be paired properly
  ;; (my-org-mode-hooks)
  :hook (org-mode . my-org-mode-hooks)
  :general
  (defun my/open-agenda (&optional arg)
    "Open org-agenda directly"
    (interactive "p")
    (org-agenda arg "a"))
  
  (defun my/open-agenda-full-todo (&optional arg)
    "Open org-agenda directly"
    (interactive "p")
    (org-agenda arg "n"))
  
  (leader
    "aa"  'my/open-agenda
    "aA"  'my/open-agenda-full-todo
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
    :keymaps '(org-mode-map) ;; should not include org-agenda-mode-map here, otherwise [RET] would not switch to item!
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
  
    "a"   'org-archive-subtree
  
    "c"  '(:ignore t :which-key "org-clock")
    "ci" 'org-clock-in
    "co" 'org-clock-out
    "cc" 'org-clock-goto
    "cu" 'org-clock-update-time-maybe
    "cm" 'org-clock-modify-effort-estimate
    "cp" 'org-pomodoro
    "cP" 'org-pomodoro-extend-last-clock
    )
  :config
  (require 'ox-gfm nil t)
  (setq org-todo-keywords
        '((sequence "ACTIVE(a)" "WAITING(w)" "TODO(t)" "|" "DONE(d)" "CANCELLED(c)" "FAILED(f)")
          (sequence "❍(W)" "☐(T)" "|" "☑(D)" "☒(C)")
          (sequence "NEXT(n)" "IN-PROGRESS(I)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)" "FAILED(f)")))
  
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
  
  ;; always indent
  (setq org-startup-indented t)
  
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
      ("md" . "src markdown")
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
  
  (defun my/tex-dollar2paren ()
    (interactive)
    (if (region-active-p)
         (save-excursion
  	 (replace-regexp "\\$\\(.*?\\)\\$" "\\\\(\\1\\\\)" nil (region-beginning) (region-end)))
      (save-excursion
        (replace-regexp "\\$\\(.*?\\)\\$" "\\\\(\\1\\\\)" nil (point-min) (point-max)))))
  
  (defun my/tex-paren2dollar ()
    (interactive)
    (if (region-active-p)
        (save-excursion
  	(replace-regexp "\\\\(\\(.*?\\)\\\\)" "$\\1$" nil (region-beginning) (region-end)))
      (save-excursion
        (replace-regexp "\\\\(\\(.*?\\)\\\\)" "$\\1$" nil (point-min) (point-max)))))
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
  	org-journal-time-prefix ""
          org-journal-start-on-weekday '7)
  
    (setq org-journal-find-file #'find-file)
  
  
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
  
    (defun get-journal-file-today ()
      "Gets filename for today's journal entry."
      (let ((month-name (format-time-string "%m")))
        (expand-file-name (concat org-journal-dir (format-time-string "/%Y/month") month-name ".org"))))
  
    ;; (defun get-journal-file-today ()
    ;;   "Gets filename for today's journal entry."
    ;;   (let ((daily-name (format-time-string "%Y%m")))
    ;;     (expand-file-name (concat org-journal-dir daily-name ".org"))))
  
    ;; (defun journal-file-today ()
    ;;   "Creates and load a journal file based on today's date."
    ;;   (interactive)
    ;;   (find-file (get-journal-file-today)))
  
    ;; source
    ;; https://isamert.net/2021/01/25/how-i-do-keep-my-days-organized-with-org-mode-and-emacs.html
    (defun my/toggle-side-journal-buffer ()
      "Toggle `bullet.org` in a side buffer for quick note taking.  The buffer is opened in side window so it can't be accidentaly removed."
      (interactive)
      (my/toggle-side-buffer-with-file (get-journal-file-today)))
  
    (defun my/buffer-visible-p (buffer)
      "Check if given BUFFER is visible or not.  BUFFER is a string representing the buffer name."
      (or (eq buffer (window-buffer (selected-window))) (get-buffer-window buffer)))
  
    (defun my/display-buffer-in-side-window (buffer)
      "Just like `display-buffer-in-side-window' but only takes a BUFFER and rest of the parameters are for my taste."
      (select-window
       (display-buffer-in-side-window
        buffer
        (list (cons 'side 'right)
              (cons 'slot 0)
              (cons 'window-width 84)
              (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                             (cons 'no-other-window nil)))))))
  
    (defun my/remove-window-with-buffer (the-buffer-name)
      "Remove window containing given THE-BUFFER-NAME."
      (mapc (lambda (window)
              (when (string-equal (buffer-name (window-buffer window)) the-buffer-name)
                (delete-window window)))
            (window-list (selected-frame))))
  
    (defun my/toggle-side-buffer-with-file (file-path)
      "Toggle FILE-PATH in a side buffer. The buffer is opened in side window so it can't be accidentaly removed."
      (interactive)
      (let ((fname (file-name-nondirectory file-path)))
      (if (my/buffer-visible-p fname)
          (my/remove-window-with-buffer fname)
        (my/display-buffer-in-side-window
         (save-window-excursion
           (find-file file-path)
           (current-buffer))))))
  
    (defun journal-file-today ()
      "Creates and load a journal file based on today's date."
      (interactive)
      (org-journal-open-current-journal-file))
  
    (defun my/open-diary ()
      (interactive)
      "Open org-diary directly"
      (journal-file-today))
  
    (leader
      "nd" 'my/open-diary
      "nt" 'my/toggle-side-journal-buffer)
  )
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
  
  ;; (add-to-list 'org-capture-templates
  ;; 	     '("d" "Daily Tasks in Journal" plain (function my/org-journal-find-location)
  ;; 	       "** Tasks [/]\nDDL: \n- [ ] \nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ]"
  ;; 	       :immediate-finish t
  ;; 	       :jump-to-captured t))
  
  (add-to-list 'org-capture-templates
  	     '("d" "Diary" plain (function my/org-journal-find-location)
  	       "daily\n"
  	       :immediate-finish t
  	       :jump-to-captured t))
  
  ;; (add-to-list 'org-capture-templates
  ;; 	     '("d" "Diary" plain (function my/org-journal-find-location)
  ;; 	       "daily\n\n** Daily Summary [/]\n- [ ] DDLs are completed. \n- [ ] Org my life. \n- [ ] Enjoyed my day."
  ;; 	       :immediate-finish t
  ;; 	       :jump-to-captured t))
  
  ;; (add-to-list 'org-capture-templates
  ;; 	     '("w" "Weekly Tasks in Journal" plain (function my/org-journal-find-location)
  ;; 	       "* Weekly Goals [/]\nDDL: \n- [ ] \nRESEARCH: \n- [ ] \nCOURSES: \n- [ ] \nSERVICES: \n- [ ] \nOTHERS: \n- [ ]"
  ;; 	       :immediate-finish t
  ;; 	       :jump-to-captured t))
  
  (add-to-list 'org-capture-templates
  	     '("p" "Proposal to write [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* ACTIVE [%^{SHORT}] %^{PROPOSAL TITLE} [/]
    :PROPERTIES:
    :COOKIE_DATA: todo recursive
    :END:\n** WAITING Prep. the budget form\n** WAITING Literature Review [/]\n** WAITING Proposal Writing [/]\n** WAITING Supplementary Doc Prep. [/]"))
  
  (add-to-list 'org-capture-templates
  	     '("r" "Research project [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* ACTIVE [%^{SHORT}] %^{PROJECT TITLE} [/]
    :PROPERTIES:
    :COOKIE_DATA: todo recursive
    :END:\n** WAITING Literature review [/]\n** WAITING Research questions [/]\n** WAITING Paper writing [/]"))
  
  (add-to-list 'org-capture-templates
  	     '("R" "Paper/Proposal to review [inbox]" entry
  	       (file+headline my-org-inbox "Tasks") "* ACTIVE [%^{SHORT}] %^{TITLE} [%]
    :PROPERTIES:
    :COOKIE_DATA: todo recursive
    :END:\n** WAITING Submission overview\n** WAITING Submission evaluation[/]\n** WAITING Review letter writing [/]"))
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
  			   (my/opened-buffer-files :maxlevel . 9)
  			   )
        )
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  
  (setq org-directory (list my-org-dir))
  (setq org-agenda-files
        (list
         my-org-inbox
         my-org-gtd
         my-org-misc
         ;; my-org-projects
         ;; my-org-teaching
         ;; my-org-reading
         ;; my-org-proposals
         ))
  
  (setq org-archive-location my-org-archive)
  
  (require 'org-agenda)
  (general-def org-agenda-mode-map
    "RET" 'org-agenda-switch-to)
  
  ;; org-agenda split on right # DOES not work sadly
  (defadvice org-agenda (around split-vertically activate)
    (let ((split-width-threshold 40)    ; or whatever width makes sense for you
          (split-height-threshold nil)) ; but never horizontally
      ad-do-it))
  
  ;; (setq org-agenda-window-setup 'reorganize-frame)
  
  ;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  
  
  (setq bibtex-dialect 'biblatex) ;;; ???? should it be here ?
  (setq org-e-latex-tables-booktabs t)
  (setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  (setq org-latex-packages-alist
      (quote (("" "parskip" t)
  	    ("" "amsmath" t)
  	    ("" "amssymb" t)
  	    ("" "amsthm" t)
  	    ("" "amsfonts" t)
  	    ("" "mathtools" t)
  	    ("" "braket" t)
  	    ("" "booktabs" t)
  	    ("" "bbm" t)
  	    ("" "listings" t)
  	    ("" "algorithm2e" t)
  	    ("" "xcolor" t)
  	    ("" "mymacros" t))))
  (add-to-list 'org-latex-classes
  	       '("notes"
  		"\\documentclass[11pt]{article}
  \\usepackage[normalem]{ulem}
  \\usepackage{booktabs}
  \\usepackage[inline, shortlabels]{enumitem}
  \\usepackage[backref=true,natbib=true,maxbibnames=99,doi=false,url=false,giveninits=true]{biblatex}
  \\usepackage{hyperref}
  \\usepackage{mynotes}
  \\usepackage{mymacros}
  [NO-DEFAULT-PACKAGES]
  [NO-PACKAGES]
  %%%% configs
  \\DefineBibliographyStrings{english}{backrefpage={page}, backrefpages={pages}}
  \\setlength\\parindent{0pt}
  \\setitemize{itemsep=1pt}"
  	    ("\\section{%s}" . "\\section*{%s}")
  	    ("\\subsection{%s}" . "\\subsection*{%s}")
  	    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
  	    '("manuscripts"
  	    "\\documentclass[11pt]{article}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage[normalem]{ulem}
  \\usepackage[margin=1in]{geometry}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage{pgf,interval}
  \\usepackage{booktabs}
  \\usepackage[inline]{enumitem}
  \\usepackage[backref=true,natbib=true,maxbibnames=99,doi=false,url=false,giveninits=true,dashed=false]{biblatex}
  \\usepackage{hyperref}
  %%%% configs
  \\DefineBibliographyStrings{english}{backrefpage={page}, backrefpages={pages}}
  \\intervalconfig{soft open fences}
  \\setlength\\parindent{0pt}
  \\setitemize{itemsep=1pt}"
  	    ("\\section{%s}" . "\\section*{%s}")
  	    ("\\subsection{%s}" . "\\subsection*{%s}")
  	    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
  	    '("slides"
  		"\\documentclass[notheorems]{beamer}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage[normalem]{ulem}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage{booktabs}
  \\usepackage[natbib=true,backend=biber,style=authoryear-icomp,maxbibnames=1,maxcitenames=2,uniquelist=false,doi=false,isbn=false,url=false,eprint=false,dashed=false]{biblatex}
  \\usepackage{pgfpages}
  %%%% configs
  \\setlength\\parindent{0pt}"
  	    ("\\section{%s}" . "\\section*{%s}")
  	    ("\\subsection{%s}" . "\\subsection*{%s}")
  	    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  
  (add-to-list 'org-latex-classes
  	    '("moderncv"
  	    "\\documentclass{moderncv}
  [NO-DEFAULT-PACKAGES]
  [NO-PACKAGES]"
  	    ("\\section{%s}" . "\\section*{%s}")
  	    ("\\subsection{%s}" . "\\subsection*{%s}")
  	    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  
  (add-to-list 'org-latex-classes
  	     '("annual report"
  		"\\documentclass{article}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage[normalem]{ulem}
  [NO-DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage{booktabs}
  \\usepackage[inline]{enumitem}
  \\usepackage{hyperref}
  "
  		("\\section{%s}" . "\\section*{%s}")
  		("\\subsection{%s}" . "\\subsection*{%s}")
  		("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  
  )

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :init
  (defun my/notify-pomo-fin ()
    (my/notify-osx "Pomodoro completed!" "Time for a break."))
  (defun my/notify-break-fin ()
    (my/notify-osx "Break finished!" "Ready for another?"))
  (defun my/notify-long-break-fin ()
    (my/notify-osx "Long break finished!" "Ready for another?"))
  (defun my/notify-pomo-kill ()
    (my/notify-osx "Pomodoro killed!" "One does not simply kill a pomodoro!!!"))
  :hook
  (org-pomodoro-finished . my/notify-pomo-fin)
  (org-pomodoro-break-finished . my/notify-break-fin)
  (org-pomodoro-long-break-finished . my/notify-long-break-fin)
  (org-pomodoro-killed . my/notify-pomo-kill)
  :config
  (setq
   org-pomodoro-length 105
   org-pomodoro-short-break-length 15
   )
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

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

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

;; (use-package anki-editor
;;   :straight (:host github :repo "louietan/anki-editor" :branch "master")
;;   :after org
;;   :config
;;   ;; I like making decks
;;   (setq anki-editor-create-decks 't
;;         anki-editor-org-tags-as-anki-tags 't)
;;   ;; (setq org-my-topo-anki-file (format "%s/%s" org-my-notes "/anki/topo.org")
;;   ;;       org-my-folland-anki-file (format "%s/%s" org-my-notes "/anki/folland.org")
;;   ;;       org-my-folland-1-anki-file (format "%s/%s" org-my-notes "/anki/folland-ch1-extras.org")
;;   ;;       org-my-folland-2-anki-file (format "%s/%s" org-my-notes "/anki/folland-ch2.org")
;;   ;;       org-my-rockafellar-6-anki-file (format "%s/%s" org-my-notes "/anki/var-analysis-ch6.org")
;;   ;;       org-my-grammar-anki-file (format "%s/%s" org-my-notes "/anki/grammar.org")
;;   ;;       org-my-analysis-anki-file (format "%s/%s" org-my-notes "/anki/analysis.org"))

;;   ;; ;; https://orgmode.org/manual/Template-expansion.html
;;   ;; (add-to-list 'org-capture-templates
;;   ;;              '("AT" "Topology Basic LaTeX"
;;   ;;                entry
;;   ;;                (file+headline org-my-topo-anki-file "Topology")
;;   ;;                "* Card %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: LaTeX Basic w. Reference\n:ANKI_DECK: Topology\n:END:\n** Front\n%?\n** Back\n\n** Remarks\n\n** Chapter\n\n** Reference\nTopology, 2nd Edition. James Munkres\n"))

;;   ;; (add-to-list 'org-capture-templates
;;   ;;              '("AF" "Folland Basic LaTeX"
;;   ;;                entry
;;   ;;                (file+headline org-my-folland-anki-file "Real Analysis by Folland")
;;   ;;                "* %^{Card Front} %^g\n:PROPERTIES:\n:ANKI_DECK: %^{Anki Deck Name|RAF::Ch|RAF::Ch.1 Measures|RAF::Ch.2 Integration|RAF::Ch.3 Differentiation}\n:ANKI_NOTE_TYPE: LaTeX Basic w. Reference\n:END:\n** Front\n%\\1 %?\n** Back\n\n** Remarks\n%^{Remarks|None}\n** Chapter\n%^{Chapter|Ch|Ch1|Ch2|Ch3}\n** Reference\nReal Analysis, 2nd Edition. Gerald B. Folland\n"))

;;   ;; (add-to-list 'org-capture-templates
;;   ;;             '("AV" "Variational Analysis Basic LaTeX"
;;   ;;                 entry
;;   ;;                 (file+headline org-my-rockafellar-6-anki-file "Variational Analysis by Rockafellar")
;;   ;;                 "* Card %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: LaTeX Basic w. Reference\n:END:\n** Front\n%?\n** Back\n\n** Remarks\n\n** Chapter\n\n** Reference\nVariational Analysis, 3rd Printing. Rockafellar and Wets\n"))

;;   ;; (add-to-list 'org-capture-templates
;;   ;;              '("AG" "English Grammar Basic LaTeX"
;;   ;;                entry
;;   ;;                (file+headline org-my-topo-grammar-file "Grammar")
;;   ;;                "* Card %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: LaTeX Basic w. Reference\n:END:\n** Front\n%?\n** Back\n\n** Remarks\n\n** Chapter\n\n** Reference\n\n"))
;;   )

(provide 'my-org)
