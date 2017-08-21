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
(load custom-file t)

;; Establish some minimal level of decency
(use-package better-defaults
  :ensure t)
(setq inhibit-startup-screen t)

;; Alphabetical henceforth

(use-package autorevert
  :ensure t
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package counsel
  :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("C-c g"   . counsel-git)
   ("C-c j"   . counsel-git-grep)
   ("C-c k"   . counsel-ag)
   ("C-x l"   . counsel-locate)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))
  
(use-package frame
  :config
  (blink-cursor-mode -1)
  (set-frame-font "Fira Code" nil t))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package server
  :ensure t
  :config
  (server-start))

