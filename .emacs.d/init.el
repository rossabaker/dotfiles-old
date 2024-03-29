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

;; Better defaults, plus better better defaults
(use-package better-defaults
  :ensure t
  :init
  (setq inhibit-startup-screen t)
  (setq-default cursor-in-non-selected-windows)
  :config
  (blink-cursor-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1))

;; Alphabetical henceforth

(use-package autorevert
  :ensure t
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (color-theme-sanityinc-tomorrow-night))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h l" . counsel-find-library)
  ("C-h S" . counsel-info-lookup-symbol)
  ("C-x 8 RET" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag))

(use-package frame
  :config
  (blink-cursor-mode -1)
  (when (window-system)
    (set-default-font "Fira Code")
    (global-unset-key (kbd "C-z"))))
  
(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :config
  (guru-global-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :bind
  ("C-c C-r" . ivy-resume))

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))

(use-package powerline
  :ensure t
  :config
  (setq-default powerline-default-separator 'butt
                powerline-height (truncate (* 1.33 (frame-char-height)))))
  
(use-package restart-emacs
  :ensure t)

(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package which-func
  :ensure t
  :config
  (setq which-func-unknown "⊥")
  (which-function-mode t))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; Finally, load the sensitive bits

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load private-file)))
