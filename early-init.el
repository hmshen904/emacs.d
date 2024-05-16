;; disable package.el
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t) ;; have use-package use straight.el by default.

;; straight.el init
;; source: https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package via straight
(straight-use-package 'use-package)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(defvar my-module-path "~/.config/emacs/modules")
