;; (use-package olivetti
;;   :diminish
;;   :commands olivetti-mode
;;   :config
;;   (setq olivetti-body-width 120)
;;   (setq olivetti-minimum-body-width 120))

(use-package visual-fill-column
  :diminish
  :commands visual-fill-column-mode
  :init
  (add-hook 'prog-mode-hook #'visual-fill-column-mode)
  (add-hook 'text-mode-hook #'visual-fill-column-mode)
  (setq visual-fill-column-width 100)
  ;; :config
  ;; (visual-fill-column-mode 1)
)

(use-package hide-mode-line
  :diminish
  :config
  (add-hook 'help-mode-hook #'hide-mode-line-mode))

(use-package mood-line
  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  :config
  (mood-line-mode))

;; add padding around mode line
;; The :style flat-button makes the border have the same color as the background of the mode line.
;; see https://www.reddit.com/r/emacs/comments/18ktlkg/padding_a_custom_mode_line_with_theme_colour/
(defun my/pad-mode-line ()
  "pad my mode-line"
  (interactive)
  (set-face-attribute 'mode-line nil
		      :box '(:line-width 4 :style flat-button))
  (set-face-attribute 'mode-line-inactive nil
		      :box '(:line-width 4 :style flat-button))
  )

(setq ns-auto-hide-menu-bar t)
(set-frame-position nil 0 -24)
(tool-bar-mode 0)
(set-frame-size nil 150 80)

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; (setq ns-use-proxy-icon nil)
;; (setq frame-title-format nil)

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
    (my/load-indexed-theme)
    (my/pad-mode-line))

  (defun my/load-indexed-theme ()
    (load-theme (nth my/themes-index my/themes)))

  ;; (defun my/load-theme ()
  ;;   (interactive)
  ;;   (load-theme)
  ;;   (my/pad-mode-line))

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
  (leader "tsa" #'load-anti-zenburn))

(use-package leuven-theme
  :config
  (defun load-leuven-light ()
      "Load the `doom-leuven' theme."
      (interactive)
      (load-theme 'leuven t)))

(load-anti-zenburn)
;; (load-leuven-light)
(my/pad-mode-line)

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(require'dired)
(setf dired-kill-when-opening-new-dired-buffer t) ;; only open one buffer (need emacs 28+)
;; Additional configurations
(setq dired-listing-switches "-alh")  ;; Show human-readable file sizes
(setq dired-dwim-target t)  ;; Enable "do what I mean" for copying and moving files

(provide 'my-ui)
