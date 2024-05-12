;;; my-bibtex.el --- My bibtex for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my bibtex for Emacs
;;; Package --- Summary
;;; Code:

(defvar my-bib-files '("~/Dropbox/40-Scripts/texmf/tex/latex/local/master.bib"))
(defvar my-pdf-library '("~/Dropbox/10-Resources/Papers/pdfs"))
(defvar my-notes '("~/Dropbox/10-Resources/Papers/notes"))
;; (defvar my-csl-styles-dir "~/Dropbox/50-Software/Zotero/styles")

(use-package citar
  :config
  (setq citar-bibliography (symbol-value 'my-bib-files))
  (setq citar-library-paths (symbol-value 'my-pdf-library))
  (setq citar-notes-paths (symbol-value 'my-notes))
  (setq org-cite-csl-styles-dir (expand-file-name "~/Dropbox/50-Software/Zotero/styles"))
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

  :general
  (leader
    "ab"  'citar-open)

  :general-config
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

(use-package biblio
  :general-config
  (local-leader bibtex-mode-map
    "i" 'biblio-doi-insert-bibtex))

(use-package bibtex-utils
  :hook (bibtex-mode . load-bibtex-utils)
  ;; :magic ("%bib" . load-bibtex-utils)
  ;; :mode ("\\.bib\\'" . load-bibtex-utils)
  :commands bu-jump-to-doc
  :config
  (defun load-bibtex-utils ()
    (progn
      (require 'bibtex-utils)
      (my-bibtex-mode-hooks)))
  (defun my-bibtex-mode-hooks ()
    (auto-fill-mode 0)
    (display-line-numbers-mode 1))
  (setq bu-pdf-dir (symbol-value 'my-pdf-library)
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-names-stretch 1
        bibtex-autokey-year-title-separator "_"
        bibtex-maintain-sorted-entries t)

  :general-config
  (general-def '(normal insert visual emacs) bibtex-mode-map
    "C-n"  'bu-next-entry
    "C-p"  'bu-previous-entry
    )
  (local-leader bibtex-mode-map
    "o" 'bu-open-doc
    "K" 'bu-make-field-keywords
    "c" 'bibtex-clean-entry)

  (local-leader
    :keymaps '(LaTeX-mode-map)
    "o" 'bu-jump-to-doc))

(provide 'my-bibtex)
;;; my-bibtex ends here
