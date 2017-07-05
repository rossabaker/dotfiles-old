(package-initialize)

;; Banish customizations to their ownfile
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "config.org"))
