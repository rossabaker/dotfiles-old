;; And I know the love I have for you
;; Will grow and grow and grow, I think
;; And so my love, I offer you
;; A love that is strong, a prune that is true
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-readable-p custom-file)
  (load custom-file))

;; This is the exciting part. This is like The Supremes.
(org-babel-load-file (concat user-emacs-directory "settings.org"))
