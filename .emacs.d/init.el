;; Packages
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(require 'tls)
(require 'gnutls)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq tls-checktrust t)
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Banish customizations to another file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package better-defaults
  :ensure t)

;; Alphabetical henceforth

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package simple
  :config
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1))

(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))
