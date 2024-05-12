;;; my-editor.el --- My editor config for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my editor for Emacs
;;; Package --- Summary
;;; Code:

(use-package which-key
  :config (which-key-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-respect-visual-line-mode t
	evil-undo-system 'undo-fu
	evil-mode-line-format nil)

  :custom
  (evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  ;; :custom
  ;; (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode) ;; globally enable evil-commentary
  )

;; * Best Practices by the author of general.ed
;; To facilitate extensibility and easy creation of wrappers, ~general-define-key~ uses keyword arguments to specify everything besides the key definitions, including for the =:states= and =:keymaps=. Since users will most often specify one or both of these keyword arguments, ~general-define-key~ is often less concise than ~define-key~ or ~evil-define-key~. It is for this reason that it is recommended that ~general-define-key~ not be used directly. =general.el= provides wrappers around ~general-define-key~ that take positional arguments like ~define-key~ and ~evil-define-key~ (~general-emacs-define-key~, ~general-evil-define-key~, and ~general-def~). It is recommended that you use these instead of ~general-define-key~. ~general-create-definer~ can also be used to create a new definer with certain default settings (e.g. prefix settings). For clarity and consistency, examples in the documentation usually use ~general-define-key~ unless the example is explicitly for a wrapper. However, [[#positional-argument-wrappers][~general-def~]] is recommended over ~general-define-key~ as it is more flexible and concise. Positional arguments are /optional but not required/, so ~general-def~ can mostly act as a drop-in replacement for many key definers (including ~general-define-key~, ~define-key~, and ~evil-define-key~). Note that ~general-create-definer~ and the =:general= keyword argument for ~use-package~ use ~general-def~. I personally only use ~general-def~.

;; Since it is more common for commands to not be sharp quoted in key definitions, this package's examples use single quotes for commands. I personally prefer to always properly sharp quote functions, so commands in the actual non-example code are always sharp quoted.

;; Although ~general-define-key~ will automatically defer keybindings until the specified keymaps exist, it is recommended you use it with ~with-eval-after-load~ or use-package's =:config= keyword instead. This is because while the deferring mechanism works, it is much slower than using ~eval-after-load~. See [[#will-generalel-slow-my-initialization-time][Will general.el slow my initialization time?]] for more information on ensuring you are not unnecessarily slowing down Emacs initialization.

;; See also the rest of [[#faq][FAQ]] for commonly asked questions

;; To summarize, my recommended usage of general.el looks like this:
;; - Use ~general-def~, other positional definers, and your own definers created with ~general-create-definer~
;; - Use =use-package= or a similar helper
;; - Use =:general= for keybindings meant to load a package
;; - Use =:general-config= or =:config= for other keybindings
;; - Do not use use the =:which-key= extended definition keyword unless you absolutely need to (see [[#which-key-integration][Which Key Integration]] for details)
;; - Follow the other recommendations in [[#will-generalel-slow-my-initialization-time][Will general.el slow my initialization time?]]

;; From a stylistic perspective (completely personal preference) I:
;; - Explicitly use the command name with =:general=, e.g. ~:general (general-def <keymap> ...)~ instead of ~:general (<keymap> ...)~. This allows individually evaling the forms or moving them elsewhere without having to change them.
;; - Sharp quote commands (e.g. ~#'execute-extended-command~) but not lambdas


;; key bindings
(use-package general
  :after evil
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  ;; (general-override-mode 1)

  ;; leader key SPC similar to spacemacs
  (general-create-definer leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC" ;; access leader in insert and mode
    )

  ;; local leader key SPC similar to spacemacs
  (general-create-definer local-leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m" ;; access local leader in insert mode
    )

  ;; some useful functions

  ;; open config directory
  (defvar user-config-dir "~/.config/emacs/modules")
  (defun open-user-config-dir ()
    "Open the `user-config-dire' in the same window"
    (interactive)
    (dired user-config-dir))

  (defun find-user-init-file ()
    "Edit the `user-init-file', in same window."
    (interactive)
    (find-file user-init-file))

  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file user-init-file))

  ;;Taken from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

  ;; https://magnus.therning.org/2023-07-09-general.el-and-two-ways-to-define-keybindings.html
  (general-def
    "C-x x" 'eval-defun)

  (leader

    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "x"   (general-simulate-key "C-x")
    "u"   '(universal-argument :wk "C-u")

    ;; Theme operations
    "t"   '(:ignore t :which-key "themes")
    "tn"  'my/cycle-theme
    "tt"  'load-theme
    "tl"  'load-leuven-theme
    "td"  'load-dichromacy-theme

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qq"  'kill-emacs
    ;; "qq"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    ;; "bb"  'mode-line-other-buffer
    "bk"  'kill-this-buffer
    "bn"  'next-buffer
    "bp"  'previous-buffer
    "b]"  'next-buffer
    "b["  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer
    "bb"  'switch-to-buffer

    ;; Window operations
    "w"   '(:ignore t :which-key "window")
    "wn"  'evil-window-vnew
    "w>"  'evil-window-increase-width
    "w<"  'evil-window-decrease-width
    "w+"  'evil-window-increase-height
    "w-"  'evil-window-increase-height
    "w/"  'evil-window-vsplit
    "wv"  'evil-window-split
    "ww"  'evil-window-next
    "wc"  'evil-window-delete
    "wD"  'delete-other-windows

    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "fc"  'write-file
    "fe"  '(:ignore t :which-key "emacs")
    "fed" 'find-user-init-file
    "feD" 'open-user-config-dir
    "feR" 'load-user-init-file
    "fd"  'dired
    "fb"  'bookmark-bmenu-list
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer

    ;; Applications
    "a"   '(:ignore t :which-key "Applications")
    ":"   'shell-command
    ";"   'eval-expression
    "ac"  'calendar

    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new
    )
  )

(use-package undo-fu
  :general
  ('normal "C-r" 'undo-fu-only-redo))

(use-package smartparens-mode
  :straight (:host github :repo "Fuco1/smartparens"
             :branch "master")
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(provide 'my-editor)
