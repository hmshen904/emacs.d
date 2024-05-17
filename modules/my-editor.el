(use-package which-key
  :diminish
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
  :diminish
  ;; :ensure t
  ;; :custom
  ;; (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :after evil
  :diminish)

(use-package evil-surround
  :after evil
  :diminish
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode) ;; globally enable evil-commentary
  )

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
  (defun open-user-config-dir ()
    "Open the `user-config-dire' in the same window"
    (interactive)
    (dired my-config-dir))

  (defun find-user-config-file ()
    "Edit the `user-config-file', in same window."
    (interactive)
    (find-file my-config-file))

  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file my-init-file))

  (defun open-iTerm-here ()
    "Open item at the current path"
    (interactive)
    (shell-command "open -a iTerm ."))

  (defun open-Finder-here ()
    "Open Finder at the current path"
    (interactive)
    (shell-command "open ."))

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

    ;; jumpers
    "j"   '(:ignore t :which-key "jump")
    "jl"  'evilem-motion-next-visual-line
    "jk"  'evilem-motion-previous-visual-line
    "jt"  'evilem-motion-find-char-to
    "jT"  'evilem-motion-find-char-to-backward
    "jf"  'evilem-motion-find-char
    "jF"  'evilem-motion-find-char-backward
    "j("  'evilem-motion-backward-sentence-begin
    "j)"  'evilem-motion-forward-sentence-begin

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
    "fed" 'open-user-config-dir
    "fec" 'find-user-config-file
    "feR" 'load-user-init-file
    "fd"  'dired
    "fb"  'bookmark-bmenu-list
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer

    ;; Applications
    ":"   'shell-command
    ";"   'eval-expression
    "a"   '(:ignore t :which-key "Applications")
    "ac"  'calendar
    "at"  'open-iTerm-here
    "af"  'open-Finder-here

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
  :diminish
  :config
  ;; load default config
  (require 'smartparens-config))

(provide 'my-editor)
