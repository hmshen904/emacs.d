(use-package magit
  :commands (magit-status)
  :general
  (leader
   "g"   '(:ignore t :which-key "git")
   "gg"  'magit-status))
(provide 'my-vc)
