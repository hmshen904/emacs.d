;;; my-org-mode.el --- org-mode customizations for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- org-mode for Emacs
;;; Package --- Summary
;;; Code:

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
  (add-hook 'org-mode-hook 'my-org-mode-hooks)
  :general
  (leader
   "oa"  'org-agenda
   "ot"  'org-todo-list)
  :config

  (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)")
            (sequence "[.](T)" "[-](p)" "[?](m)" "|" "[X](D)")
            (sequence "NEXT(n)" "INPROGRESS(I)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)" "FAILED(f)")))

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
	org-log-done 'time
	org-log-into-drawer t
	org-latex-prefer-user-labels t
	;; Downscale image size
	;; Source: https://emacs.stackexchange.com/questions/26363/downscaling-inline-images-in-org-mode
	org-image-actual-width nil
	org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "REPORT")
	org-agenda-start-on-weekday 0
        ;; https://stackoverflow.com/questions/17239273/org-mode-buffer-latex-syntax-highlighting
	org-highlight-latex-and-related '(latex script entities)
        ;; https://stackoverflow.com/questions/11365739/how-to-cancel-the-hypersetup-in-0rg-mode-of-emacs
	org-latex-with-hyperref nil)

  (setq org-emphasis-alist
	'(("*" (bold :foreground "Orange" ))
	  ("/" italic)
	  ("_" underline)
	  ("=" (:background "maroon" :foreground "white"))
	  ("~" (:background "deep sky blue" :foreground "MidnightBlue"))))

  (add-to-list 'org-babel-load-languages '(ledger . t))

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

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

  (setq org-id-locations-file "~/.config/emacs/.org-id-locations")

  (defun org-babel-execute-and-next ()
    (interactive)
    (progn (org-babel-execute-src-block)
           (org-babel-next-src-block)))
  (setq org-highlight-latex-and-related '(entities script latex)
        org-tags-column 90)

  (local-leader org-mode-map
    ;; :keymaps 'org-mode-map
    "e"   'org-export-dispatch
    "t"   'org-hide-block-toggle
    "x"   'org-babel-execute-src-block
    "X"   'org-babel-execute-and-next
    "i"   'org-toggle-inline-images
    "d"   'org-babel-remove-result
    "o"   'org-open-at-point)
)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-re-reveal
  :hook (org-mode . load-org-re-reveal)
  :config
  (defun load-org-re-reveal ()
    (require 'org-re-reveal))
  (setq org-re-reveal-root "file:///some_loc/reveal.js")) ;; set_this

(provide 'my-org-mode)
;;; my-org-mode ends here
