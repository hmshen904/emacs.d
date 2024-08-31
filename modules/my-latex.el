(use-package auctex
  :no-require t
  ;; :hook (LaTeX-mode . visual-fill-column-mode)
  ;; :hook (LaTeX-mode . olivetti-mode)
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

  ;; (setq TeX-quote-after-quote t)

  (setq TeX-view-program-list
	'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
	  ("PDF Tools" TeX-pdf-tools-sync-view)
	  ("preview-pane" latex-preview-pane-mode)))

  (setq TeX-view-program-selection
        '((output-pdf "Skim")
	  (output-pdf "PDF Tools")
          (output-dvi "open")
          (output-pdf "open")
          (output-html "open")
          (output-pdf "preview-pane")))
  )

(use-package tex
  :straight auctex
  :config
  (defun my-LaTeX-mode-hooks ()
    (latex-electric-env-pair-mode)
    (whitespace-mode)
    (show-paren-mode)
    (visual-line-mode)
    (flyspell-mode)
    (outline-minor-mode)
    (display-line-numbers-mode t)
    (TeX-source-correlate-mode t)
    (electric-indent-local-mode -1)
    (prettify-symbols-mode))
  (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hooks)
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

  ;; (setq TeX-electric-math (cons "\\(" "\\)"))
  (setq LaTeX-electric-left-right-brace t)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (setq TeX-command-default "LaTeXmk")
  (local-leader LaTeX-mode-map
    "P" 'preview-at-point
    "m" 'TeX-command-master
    "c" 'TeX-command-run-all
    "v" 'TeX-view
    )
)

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package cdlatex
  :init
  (setq cdlatex-takeover-parenthesis nil)
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode   . turn-on-org-cdlatex)
  :diminish
  :config
  ;; (add-to-list 'cdlatex-parens-pairs '("\\(" . "\\)"))
  (setq cdlatex-use-dollar-to-ensure-math nil)
  ;; (setq cdlatex-paired-parens "$([{|")
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
          (?\" ("\\dbquot" "" ""))
          )
        cdlatex-math-modify-alist
        '(
          (?b "\\bm" nil t nil nil)
          (?B "\\mathbb" nil t nil nil)
          (?n "\\norm" nil t nil nil)
          (?a "\\abs" nil t nil nil)
          (?- "\\overline" nil t nil nil)
          ;; (?0 "\\text" nil t nil nil) % 't does the job
	  )
	)

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

  :general-config
  (general-def
   :states '(normal insert)
   :keymaps '(cdlatex-mode-map org-mode-map)
   "M-;" 'cdlatex-tab)
  )

;; (defvar my-bib-files '("~/Dropbox/40-Scripts/texmf/tex/latex/local/master.bib"))
;; (defvar my-pdf-library '("~/Dropbox/10-Resources/Papers/pdfs"))
;; (defvar my-notes '("~/Dropbox/10-Resources/Papers/notes"))

(use-package citar
  :config
  (setq citar-bibliography (list my-bib-files))
  (setq citar-library-paths (list my-pdf-library))
  (setq citar-notes-paths (list my-notes))
  (setq org-cite-csl-styles-dir my-cite-csl-styles-dir)
  (setq citar-symbol-separator " ")
  (setq citar-symbols
   `((file "⌘" . " ") (note "✎" . " ") (link "⚓" . " ")))
  (setq citar-at-point-function 'embark-act)
  (setq citar-templates
	'((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
	  (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
	  (preview . "${author::%etal} (${date year issued:4}) ${title}\n")
	  (default-preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
	  (note . "Notes on ${author editor:%etal}, ${title}")))

  (with-eval-after-load 'oc
    (setq org-cite-insert-processor 'citar)
    (setq org-cite-follow-processor 'citar)
    (setq org-cite-activate-processor 'citar)
    (setq org-cite-global-bibliography citar-bibliography)
  )

  ;; taken from citar wiki, doesn't yet work because file-name-concat is missing


  ;; (defun citar--add-file-to-library (key)
  ;;   "Add a file to the library for KEY.
  ;; The FILE can be added either from an open buffer, a file, or a
  ;; URL."
  ;;   (let* ((source
  ;;           (char-to-string
  ;;            (read-char-choice
  ;;             "Add file from [b]uffer, [f]ile, or [u]rl? " '(?b ?f ?u))))
  ;;          (directory (if (cdr citar-library-paths)
  ;;                         (completing-read "Directory: " citar-library-paths)
  ;;                       (car citar-library-paths)))
  ;;          (file-path
  ;;           (file-name-concat directory (concat key ".pdf")))) ; FIX so don't hardcode extension
  ;;     (pcase source
  ;;       ("b"
  ;;        (with-current-buffer (read-buffer-to-switch "Add file buffer: ")
  ;;          (write-file file-path)))
  ;;       ("f"
  ;;        (copy-file
  ;;         (expand-file-name
  ;;          (read-file-name "Add file: " nil nil t)) file-path))
  ;;       ("u"
  ;;        (url-copy-file (read-string "Add file URL: ") file-path)))))

  ;; (defun citar-add-file-to-library (key-entry)
  ;;   "Add a file to the library for KEY-ENTRY.
  ;; The FILE can be added either from an open buffer, a file, or a
  ;; URL."
  ;;   (interactive (list (citar-select-ref
  ;;                       :rebuild-cache current-prefix-arg)))
  ;;    (citar--add-file-to-library (car key-entry)))

  :general ;; use :general to activate a mode
  ;; :general-config ;; use :general-config when the mode is activated

  (leader
    "ab"  'citar-open)
  (local-leader
    :keymaps '(org-mode-map LaTeX-mode-map)
    "b"  '(:ignore t :which-key "bibliography")
    "bb" 'citar-open
    "bi" 'citar-insert-citation
    "br" 'citar-insert-reference
    "bk" 'citar-insert-keys
    "bn" 'citar-open-notes
    "bo" 'citar-open-files
    "bl" 'citar-open-links
    ;; "c"  'citar-insert-citation
    )
  )

;; (use-package biblio
;;   :general-config
;;   (local-leader bibtex-mode-map
;;     "i" 'biblio-doi-insert-bibtex))

;; (use-package bibtex-utils
;;   :hook (bibtex-mode . load-bibtex-utils)
;;   ;; :magic ("%bib" . load-bibtex-utils)
;;   ;; :mode ("\\.bib\\'" . load-bibtex-utils)
;;   :commands bu-jump-to-doc
;;   :config
;;   (defun load-bibtex-utils ()
;;     (progn
;;       (require 'bibtex-utils)
;;       (my-bibtex-mode-hooks)))
;;   (defun my-bibtex-mode-hooks ()
;;     (auto-fill-mode 0)
;;     (display-line-numbers-mode 1))
;;   (setq bu-pdf-dir (symbol-value 'my-pdf-library)
;;         bibtex-autokey-titlewords 2
;;         bibtex-autokey-titlewords-stretch 0
;;         bibtex-autokey-names-stretch 1
;;         bibtex-autokey-year-title-separator "_"
;;         bibtex-maintain-sorted-entries t)

;;   :general

;;   (general-def '(normal insert visual emacs) bibtex-mode-map
;;     "C-n"  'bu-next-entry
;;     "C-p"  'bu-previous-entry
;;     )
;;   (local-leader bibtex-mode-map
;;     "o" 'bu-open-doc
;;     "K" 'bu-make-field-keywords
;;     "c" 'bibtex-clean-entry)

;;   (local-leader
;;     :keymaps '(LaTeX-mode-map)
;;     "o" 'bu-jump-to-doc))

(use-package reftex
  :straight (:type built-in)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (setq reftex-plug-into-AUCTeX t
	reftex-default-bibliography (symbol-value 'my-bib-files))
  (local-leader
    :keymaps '(LaTeX-mode-map)
    ";"  'reftex-toc
    "r"  '(:ignore t :which-key "ref&cite")
    "rr"  'reftex-reference
    "rc" 'reftex-cite
    "rp" 'reftex-citep
    "rt" 'reftex-citet
    )
  ;; (local-leader LaTeX-mode-map
  ;;     "r"   'reftex-reference)
)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
)

(provide 'my-latex)
