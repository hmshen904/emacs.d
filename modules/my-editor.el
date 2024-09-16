(use-package which-key
  :diminish
  :config (which-key-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil ;; https://github.com/emacs-evil/evil-collection/issues/60
      evil-respect-visual-line-mode t
      evil-undo-system 'undo-fu
      evil-mode-line-format nil)
  :custom
  (evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  :config
  (fset 'evil-visual-update-x-selection-p 'ignore)
  (setq evil-want-change-word-to-end nil
      evil-kill-on-visual-paste nil
      evil-want-keybinding nil
      evil-symbol-word-search t)
  ;; (evil-set-initial-state 'org-agenda-mode 'motion) this does not work properly

  (evil-mode 1)
  )

(use-package evil-org
  :ensure t
  :diminish
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :after evil
  :diminish
  :ensure t
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

  (defun my/open-inbox ()
    (interactive)
    "Open inbox directly"
    (find-file my-org-inbox))

  (defun my/open-gtd ()
    (interactive)
    "Open org-my-gtd directly"
    (find-file my-org-gtd))

  (defun my/open-misc ()
    (interactive)
    "Open org-my-misc directly"
    (find-file my-org-misc))

  (defun my/open-roam-index ()
    (interactive)
    "Open my-org-roam-index directly"
    (find-file my-org-roam-index))

  (defun my/open-projects ()
    (interactive)
    "Open org-research directly"
    (find-file my-org-projects))

  (defun my/open-readings ()
    (interactive)
    "Open org-readings directly"
    (find-file my-org-reading))

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
    ;; https://www.reddit.com/r/emacs/comments/3e1ozx/acejumpmode_is_dead_long_live_avy/
    "jj"  'avy-goto-word-1
    "jl"  'avy-goto-line
    "jn"  'evilem-motion-next-visual-line
    "jp"  'evilem-motion-previous-visual-line
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

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qq"  'kill-emacs
    ;; "qq"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    ;; "bb"  'mode-line-other-buffer
    "bk"  'kill-this-buffer
    "b]"  'next-buffer
    "b["  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer
    ;; "bB"  'switch-to-buffer ;; see consult-buffer
    "bi"  'ibuffer

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
    "fm"  'bookmark-set
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer
    "RET" 'bookmark-bmenu-list

    ;; Org mode

    "n"   '(:ignore t :which-key "notes")
    "ni"  'my/open-inbox
    "ng"  'my/open-gtd
    ;; "np"  'my/open-projects
    ;; "nr"  'my/open-readings
    "nn"  'my/open-roam-index
    "nh"  'my/open-misc

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
  :defer t
  :straight (:host github :repo "Fuco1/smartparens"
             :branch "master")
  :hook
  (prog-mode LaTeX-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :diminish smartparens-mode
  :commands (smartparens-mode show-smartparens-mode)
  :config
  ;; load default config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  ;; (setq sp-ignore-modes-list
  ;;       (append sp-ignore-modes-list
  ;;               '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))

  ;; (defun my-latex-smartparens-config ()
  ;;   (sp-local-pair '(tex-mode plain-tex-mode TeX-mode latex-mode LaTeX-mode)
  ;; 		   "``" "''"
  ;;                  :trigger "\""
  ;;                  :pre-handlers  '(sp-latex-pre-slurp-handler)
  ;;                  :post-handlers '(sp-latex-skip-double-quote)))

  ;; (sp-with-modes 'LaTeX-mode
  ;;   (sp-local-pair "``" "''"
  ;; 		   :trigger "\""
  ;; 		   :pre-handlers  '(sp-latex-pre-slurp-handler)
  ;; 		   :post-handlers '(sp-latex-skip-double-quote)))

  ;; (sp-local-pair '(tex-mode plain-tex-mode TeX-mode latex-mode LaTeX-mode)
  ;;                "``" "''"
  ;;                :trigger "\""
  ;;                :pre-handlers  '(sp-latex-pre-slurp-handler)
  ;;                :post-handlers '(sp-latex-skip-double-quote))

  ;; https://emacs.stackexchange.com/questions/31166/smartparens-not-insert-pair-of-latex-quotes
  ;; (sp-local-pair '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
  ;;                "``" "''"
  ;;                :trigger "\""
  ;;                :unless '(sp-latex-point-after-backslash
  ;;                          sp-point-before-word-p
  ;;                          sp-point-after-word-p)
  ;;                :pre-handlers  '(sp-latex-pre-slurp-handler)
  ;;                :post-handlers '(sp-latex-skip-double-quote))

  ;; (sp-local-pair '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
  ;;                "`" "'"
  ;;                :trigger "'"
  ;;                :unless '(sp-latex-point-after-backslash
  ;;                          sp-point-before-word-p
  ;;                          sp-point-after-word-p)
  ;;                :pre-handlers  '(sp-latex-pre-slurp-handler)
  ;;                :post-handlers '(sp-latex-skip-double-quote))

  )

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(defun remove-electric-indent-mode ()
  (electric-indent-local-mode -1))

(use-package yasnippet
  :ensure t
  :hook ((org-mode
  	;; text-mode
          ;; prog-mode
          ;; conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets")
  :config
  ;; source https://stackoverflow.com/questions/10211730/insert-yasnippet-by-name
  (defun yas/insert-by-name (name)
    (flet ((dummy-prompt
  	  (prompt choices &optional display-fn)
  	  (declare (ignore prompt))
  	  (or (find name choices :key display-fn :test #'string=)
  	      (throw 'notfound nil))))
  	(let ((yas/prompt-functions '(dummy-prompt)))
  	  (catch 'notfound
  	    (yas/insert-snippet t)))))
  (yas-reload-all)
  )

(provide 'my-editor)
