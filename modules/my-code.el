(defun my-prog-mode-hook ()
  ;; (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  (electric-pair-mode -1)
  (flymake-mode)
  (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

;; (use-package web-mode
;;   :mode ("\\.html\\'" . web-mode)
;;   :config
;;   (setq web-mode-enable-current-column-highlight t))

(use-package emmet-mode
  :hook (web-mode  . emmet-mode)
        (css-mode  . emmet-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun my-prog-mode-hook ()
  ;; (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  (electric-pair-mode -1)
  (flymake-mode)
  (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

;; (use-package web-mode
;;   :mode ("\\.html\\'" . web-mode)
;;   :config
;;   (setq web-mode-enable-current-column-highlight t))

(use-package emmet-mode
  :hook (web-mode  . emmet-mode)
        (css-mode  . emmet-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'my-code)
