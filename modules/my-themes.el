;;; my-themes.el --- Eye candy for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- themes for Emacs
;;; Package --- Summary
;;; Code:

(with-eval-after-load 'general
  (defun disable-all-themes ()
    "disable all active themes."
    (dolist (i custom-enabled-themes)
      (disable-theme i)))

  (defadvice load-theme (before disable-themes-first activate)
    (disable-all-themes))

  ;; Following lines to cycle through themes adapted from ivan's answer on
  ;; https://emacs.stackexchange.com/questions/24088/make-a-function-to-toggle-themes
  (setq my/themes (custom-available-themes))
  (setq my/themes-index 0)

  (defun my/cycle-theme ()
    "Cycles through my themes."
    (interactive)
    (setq my/themes-index (% (1+ my/themes-index) (length my/themes)))
    (my/load-indexed-theme))

  (defun my/load-indexed-theme ()
    (load-theme (nth my/themes-index my/themes)))

  (leader "t"   '(:ignore t :which-key "themes")
          "tn"  'my/cycle-theme
          "tt"  'load-theme)
)

(use-package doom-themes
  ;; :hook (after-init . load-doom-one-light)
  :config

  (defun load-doom-one-light ()
      "Load the `doom-one-light' theme."
      (interactive)
      (load-theme 'doom-one-light))

  (defun load-doom-solarized-dark ()
      "Load the `doom-solarized-dark' theme."
      (interactive)
      (load-theme 'doom-solarized-dark))

  (defun load-doom-solarized-light ()
      "Load the `doom-solarized-light' theme."
      (interactive)
      (load-theme 'doom-solarized-light))

  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)

  ;; (leader "tsl" 'load-doom-solarized-light
  ;;         "tsd" 'load-doom-solarized-dark)
)

(use-package anti-zenburn-theme
  :config

  (defun load-anti-zenburn ()
      "Load the `doom-anti-zenburn' theme."
      (interactive)
      (load-theme 'anti-zenburn t))
  (leader "tsa" #'load-anti-zenburn)
  )

(use-package leuven-theme
  :config
  (defun load-leuven-light ()
      "Load the `doom-leuven' theme."
      (interactive)
      (load-theme 'leuven t)))

;; (load-leuven-light) ;; its modeline does not plays well with visual-fill-column

(use-package hide-mode-line
  :diminish
  :config
  (add-hook 'help-mode-hook #'hide-mode-line-mode))

;; modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25))

(use-package olivetti
  :diminish
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 120)
  (setq olivetti-minimum-body-width 120))

;; I could not make the visual-fill-column mode work with my modeline
;; (use-package visual-fill-column
;;   :config
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-adjust-for-text-scale t)
;;   (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(load-anti-zenburn)
;; (load-leuven-light)

(provide 'my-themes)
;;; my-themes ends here
