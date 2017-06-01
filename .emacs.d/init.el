;; Your text editor is malware
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

;; Package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
      			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-always-ensure t)

;; Banish custom to a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-readable-p custom-file)
  (load custom-file))

;; Strip away some unsavory defaults
(use-package "better-defaults")
(blink-cursor-mode -1)
(fset 'yes-or-no-p #'y-or-n-p)
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)

;; Font

(when (window-system)
  (set-frame-font "Fira Code" t t))
(when (functionp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

;; Packages

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))
  
(use-package beacon
  :init (beacon-mode)
  :bind (("C-c b" . beacon-blink))
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'sbt-mode))

(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t
        magic-completing-read-function 'ivy-completing-read)
  :bind (("C-c f" . counsel-git)
         ("C-c l" . counsel-locate)
         ("C-c s" . counsel-git-grep)
         ("C-h f" . counsel-describe-function)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h l" . counsel-find-library)
         ("C-h u" . counsel-unicode-char)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night :no-confirm))

(use-package delsel
  :init (delete-selection-mode))

(use-package ensime
  :pin melpa-stable)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package flyspell
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(use-package go-mode)

(use-package ivy
  :init (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
        magic-completing-read-function 'ivy-completing-read))

(use-package json-mode)

(use-package linum
  :config
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package markdown-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package saveplace
  :init (save-place-mode))

(use-package scala-mode
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (subword-mode)
              (scala-mode:goto-start-of-code))))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

 (use-package spaceline
  :init (require 'spaceline-config)
  :config
  (spaceline-emacs-theme)) 

(use-package web-mode
  :config
  (setq web-mode-engines-alist '(("go" . "\\.template\\'"))))

(use-package yaml-mode)
