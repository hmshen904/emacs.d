(use-package emacs
  :init
  ;; sigh
  (defalias 'yes-or-no-p 'y-or-n-p)

  ; which directory to put backup files
  (setq backup-directory-alist `(("." . "~/.config/emacs/backups")))

  ;transform backups file name
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

  ; toggle wrapping text at the 80th character
  (setq fill-column 80)
  (setq-default word-wrap t)

  ; turn off alarm
  (setq ring-bell-function 'ignore)

  ; disable scroll bar
  (scroll-bar-mode -1)

  ; do not display splash screen on startup
  (setq inhibit-splash-screen t)

  ; redisplay never recenters cursor
  (setq scroll-conservatively 101)

  ; spell checker
  (setq ispell-program-name "hunspell")

  ;; help window modifications
  (setq help-window-select t)
  (customize-set-variable
        'display-buffer-alist
        '(("\\*Help\\*" display-buffer-below-selected)))

  ;; relative line numbers
  (with-eval-after-load 'display-line-numbers
    (setq display-line-numbers-type 'relative
          display-line-numbers-width-start t))

  ;; tabs are evil
  (setq indent-tabs-mode nil)

  ;; always follow symlinks in git dirs
  (setq vc-follow-symlinks t)

  ;; whitespace
  (setq whitespace-style '(face trailing))

  ;; utf8 in every nook and cranny
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; persist a custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
  (load custom-file))

  ; fix =defvar= warnings
  (setq enable-local-variables :all)

  ;; use trash-cli rather than rm when deleting files.
  (setq delete-by-moving-to-trash t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  ;; font!
  (add-to-list 'default-frame-alist '(font . "SF Mono-13"))

  ; default modes
  (global-hl-line-mode 1)
  (blink-cursor-mode 0)
  (recentf-mode 1)
  (show-paren-mode t)
  (flymake-mode -1)

  ;; set_this
  ;; (setq initial-scratch-message
  ;;       (concat
  ;;        (shell-command-to-string
  ;;         "fortune calvin| cowsay -f calvin") "emacs-init-time: " (emacs-init-time)))

  :delight
  (auto-fill-function " AF")
  (visual-line-mode))

(use-package diminish)

(setq my-config-dir "~/.config/emacs")
(setq my-org-dir "~/Dropbox/Org")

(setq my-config-file (concat (file-name-as-directory my-config-dir) "config.org"))
(setq my-init-file (concat (file-name-as-directory my-config-dir) "init.el"))
(setq my-journal-dir (concat (file-name-as-directory my-org-dir) "journal"))
(setq my-org-inbox (concat (file-name-as-directory my-org-dir) "inbox.org"))
(setq my-org-gtd (concat (file-name-as-directory my-org-dir) "gtd.org"))
(setq my-org-projects (concat (file-name-as-directory my-org-dir) "projects.org"))
(setq my-org-reading (concat (file-name-as-directory my-org-dir) "readings.org"))
(setq my-org-proposals (concat (file-name-as-directory my-org-dir) "proposals.org"))
(setq my-org-teaching (concat (file-name-as-directory my-org-dir) "teaching.org"))
(setq my-org-misc (concat (file-name-as-directory my-org-dir) "misc.org"))
(setq my-org-archive (concat (file-name-as-directory my-org-dir) "archives/archives.org"))

(provide 'my-defaults)
