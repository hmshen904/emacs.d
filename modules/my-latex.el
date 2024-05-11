;;; my-latex.el --- My latex for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my latex for Emacs
;;; Package --- Summary
;;; Code:

(use-package auctex
  :no-require t
  :hook (LaTeX-mode . electric-pair-mode)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server nil
        TeX-electric-sub-and-superscript t
        TeX-engine 'xetex ;; use xelatex by default
        TeX-save-query nil)

  (setq TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)
          ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
          ("preview-pane" latex-preview-pane-mode)))

  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")
          (output-pdf "Skim")
          (output-dvi "open")
          (output-pdf "open")
          (output-html "open")
          (output-pdf "preview-pane")))
  )

(use-package tex
  :straight auctex
  :config
  (defun my-LaTeX-mode-hooks ()
    (whitespace-mode)
    (show-paren-mode)
    (visual-line-mode)
    (flyspell-mode)
    (outline-minor-mode)
    (display-line-numbers-mode t)
    (TeX-source-correlate-mode t)
    (prettify-symbols-mode))
  (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hooks)
  (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

  (local-leader LaTeX-mode-map
    "p" 'preview-at-point
    "b" 'TeX-command-master
    "a" 'TeX-command-run-all
    "v" 'TeX-view
    )
)

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

;; (use-package aas
;;   :hook (LaTeX-mode . aas-activate-for-major-mode)
;;   :hook (org-mode . aas-activate-for-major-mode))

;; (use-package laas
;;   :hook (LaTeX-mode . laas-mode)
;;   :config ; do whatever here
;;   (aas-set-snippets 'laas-mode
;;                     ;; ;; set condition!
;;                     ;; :cond #'texmathp ; expand only while in math
;;                     ;; "supp" "\\supp"
;;                     ;; "On" "O(n)"
;;                     ;; "O1" "O(1)"
;;                     ;; "Olog" "O(\\log n)"
;;                     ;; "Olon" "O(n \\log n)"
;;                     ;; ;; bind to functions!
;;                     ;; "Sum" (lambda () (interactive)
;;                     ;;         (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
;;                     ;; "Span" (lambda () (interactive)
;;                     ;;          (yas-expand-snippet "\\Span($1)$0"))

;;                     ;; add accent snippets
;;                     :cond #'laas-object-on-left-condition
;;                     "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode   . turn-on-org-cdlatex)
  :config
  (add-to-list 'cdlatex-parens-pairs '("\\(" . "\\)"))
  (setq cdlatex-math-symbol-alist
        '(
          (?0 ("\\varnothing" "\\emptyset" ""))
          (?{ ("\\min" "\\inf" ""))
          (?} ("\\max" "\\sup" ""))
          (?< ("\\subseteq" "\\subset" ""))
          (?> ("\\supseteq" "\\supset" ""))
          (?D  ("\\Delta" "\\nabla" "\\displaystyle"))
          (?f ("\\phi" "\\varphi" ""))
          (?F ("\\Phi" "" ""))
          (?I ("\\int\\limits" "" ""))
          (?: ("\\colon" "" ""))
          (?H ("\\hop" "" ""))
          (?T ("\\top" "" ""))
          )
        cdlatex-math-modify-alist
        '(
          (?b "\\bm" nil t nil nil)
          (?B "\\mathbb" nil t nil nil)
          (?n "\\norm" nil t nil nil)
          (?a "\\abs" nil t nil nil)
          (?- "\\overline" nil t nil nil)
          (?0 "\\text" nil t nil nil)))

  (setq cdlatex-env-alist
        '(
          ("axiom" "\\begin{axiom}\n?\n\\end{axiom}\n" nil)
          ("proof" "\\begin{proof}\n?\n\\end{proof}\n" nil)
          ("lemma" "\\begin{lemma}\n?\n\\end{lemma}\n" nil)
          ("theorem" "\\begin{theorem}\n?\n\\end{theorem}\n" nil)
          ("corollary" "\\begin{corollary}\n?\n\\end{corollary}\n" nil)
          ("proposition" "\\begin{proposition}\n\n\\end{proposition}\n" nil)
          ("problem" "\\begin{problem}\n?\n\\end{problem}\n" nil)
          ("solution" "\\begin{solution}\n?\n\\end{solution}\n" nil)
          ("remark" "\\begin{remark}\n?\n\\end{remark}\n" nil)
          ("comment" "\\begin{comment}\n?\n\\end{comment}\n" nil)
          ("aligned" "\\begin{aligned}\n?\n\\end{aligned}\n" nil)
          ("figure" "\\begin{figure}[!htbp]\n\\centering\n\\includegraphics[width=\\textwidth]{AUTOFILE}\n\\caption{AUTOLABEL ?}\n\\end{figure}" nil)
          ("frame" "\\begin{frame}\n\\frametitle{?}\n\\end{frame}\n" nil)
          ("frtwcl" "\\begin{frame}\n\\frametitle{?}\n\\begin{columns}\n\\begin{column}{0.5\\textwidth}\n\n\\end{column}\n\\begin{column}{0.5\\textwidth}\n\n\\end{column}\n\\end{columns}\n\\end{frame}\n" nil)
          ("twcl" "\\begin{columns}\n\\begin{column}{0.5\\textwidth}\n\n\\end{column}\n\\begin{column}{0.5\\textwidth}\n\n\\end{column}\n\\end{columns}" nil)
          ("comment box" "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n% ?\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%", nil)
          )
        cdlatex-command-alist
        '(
          ("cmtb" "Insert Comment Box" "" cdlatex-environment ("comment box") t nil)
          ("Set" "Insert \\Set{}" "\\Set{?}" cdlatex-position-cursor nil nil t)
          ("set" "Insert \\set{}" "\\set{?}" cdlatex-position-cursor nil nil t)
          ("para" "Insert \\paragraph{}" "\\paragraph{?}" cdlatex-position-cursor nil t nil)
          ("inprod" "Insert \\inprod{}{}" "\\inprod{?}{}" cdlatex-position-cursor nil nil t)
          ("non" "Insert \\nonumber\\\\" "\\nonumber\\\\\n" nil nil nil t)
          ("alid" "Insert aligned env" "" cdlatex-environment ("aligned") t nil)
          ("axm" "Insert axiom env" "" cdlatex-environment ("axiom") t nil)
          ("thm" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
          ("lem" "Insert lemma env" "" cdlatex-environment ("lemma") t nil)
          ("cor" "Insert corollary env" "" cdlatex-environment ("corollary") t nil)
          ("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
          ;; ("prob" "Insert problem env" "" cdlatex-environment ("problem") t nil)
          ("sol" "Insert solution env" "" cdlatex-environment ("solution") t nil)
          ("cmt" "Insert comment env" "" cdlatex-environment ("comment") t nil)
          ("rmk" "Insert remark env" "" cdlatex-environment ("remark") t nil)
          ("frm" "Insert frame env" "" cdlatex-environment ("frame") t nil)
          ("frtwcl" "Insert two columned frame env" "" cdlatex-environment ("frtwcl") t nil)
          ("twcl" "Insert two columns env" "" cdlatex-environment ("twcl") t nil))
        )

  (general-define-key
   :states '(normal insert)
   :keymaps 'cdlatex-mode-map
   "M-;" 'cdlatex-tab)
  )

(require 'my-bibtex)

(use-package reftex
  :straight (:type built-in)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (setq reftex-plug-into-AUCTeX t
      reftex-default-bibliography (symbol-value 'my-bib-files))
  ;; (local-leader LaTeX-mode-map
  ;;     "r"   'reftex-reference)
)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
)

(provide 'my-latex)
;;; my-latex ends here
