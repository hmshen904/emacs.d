(use-package vertico
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode)
  (leader
   "SPC" 'execute-extended-command
   ;; "bm"  'switch-to-buffer
   "ff"  'find-file))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :straight (:type built-in)
  :config
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

(use-package embark
  :general
  ("C-." 'embark-act)          ;; pick some comfortable binding
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; stolen from https://github.com/patrl/emacs.d
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
  The which-key help message will show the type and value of the
  current target followed by an ellipsis if there are further
  targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
    '(embark-which-key-indicator
      embark-highlight-indicator
      embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  )

(use-package consult
  :general
  (leader
   "fr"  'consult-recent-file
   ;; "bb"  'consult-buffer
   "fL"  'consult-locate))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t) ;; allows cycling through candidates
  (corfu-auto t) ;; disables auto-completion
  (corfu-quit-at-boundary nil) ;; needed to use orderless completion with corfu
  :init
  (global-corfu-mode)
  :config
  (general-def :keymaps 'corfu-map
    "C-n" 'corfu-next
    "C-p" 'corfu-previous))

(provide 'my-completion)

(use-package projectile
  :ensure
  :defer 0.2
  :general
  (leader
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "bb" '(projectile-switch-to-buffer :which-key "switch buffer")
    "bn" '(projectile-next-project-buffer :which-key "next project buffer")
    "bp" '(projectile-previous-project-buffer :which-key "previous project buffer")

    ;; Projects
    "p"   '(:ignore t :which-key "projects")
    ;; "p <escape>" '(keyboard-escape-quit :which-key t)
    "pc" '(projectile-compile-project :which-key "compile project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pa" '(projectile-add-known-project :which-key "add project")
    "pr" '(projectile-remove-known-project :which-key "remove project"))
  :init
  (projectile-mode +1)
  (projectile-register-project-type 'latex '(".latexmkrc" "main.tex")
				    :compile "latexmk -pdf"
				    :test "latexmk -pdf"
				    :run "evince main.pdf"
				    :test-suffix ".tex")
  )
