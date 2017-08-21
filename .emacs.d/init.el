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

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t)
  :bind
  (("C-c b" . beacon-blink)))

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

(use-package crux
  :ensure t
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :bind
  (("C-M-z"   . crux-indent-defun)
   ("C-a"     . crux-move-beginning-of-line)
   ("C-c D"   . crux-delete-file-and-buffer)
   ("C-c I"   . crux-find-user-init-file)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("C-c S"   . crux-find-shell-init-file)  
   ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-c ^"   . crux-top-join-line)
   ("C-c d"   . crux-duplicate-current-line-or-region)
   ("C-c e"   . crux-eval-and-replace)
   ("C-c f"   . crux-recentf-find-file)
   ("C-c n"   . crux-cleanup-buffer-or-region)
   ("C-c o"   . crux-open-with)
   ("C-c r"   . crux-rename-file-and-buffer)
   ("C-c t"   . crux-visit-term-buffer)
   ("C-c u"   . crux-view-url)
   ("C-k"     . crux-smart-kill-line)
   ("C-o"     . crux-smart-open-line)
   ("C-x 4 t" . crux-transpose-windows)
   ;;("C-S-Ret" . crux-smart-open-line-above)
   ;;("C-c i" . crux-ispell-word-then-abbrev)
   ;;("C-c k" . crux-kill-other-buffers)
   ;;("S-k" . crux-kill-whole-line)
   ))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package frame
  :init
  (defun ross/maybe-suspend-frame ()
    (interactive)
    (if window-system
        (message "suspend-frame disabled when on window system")
      (suspend-frame)))
  :config
  (blink-cursor-mode -1)
  (set-frame-font "Fira Code" nil t)
  :bind
  (("C-z" . ross/maybe-suspend-frame)))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package magit
  :commands magit-dispatch-popup magit-status
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))

(use-package recentf
  :ensure t
  :init
  (setq recentf-max-saved-items 200)
  :config
  (recentf-mode +1))

(use-package savehist
  :ensure t
  :init
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60)
  :config
  (savehist-mode 1))

(use-package saveplace
  :ensure t
  :config
  (save-place-mode 1))

(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package simple
  :config
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup))

(use-package which-key
  :ensure t
  :config
  (which-key-mode t))

(use-package zop-to-char
  :commands zop-to-char zop-up-to-char
  :bind
  (("M-z" . zop-up-to-char)
   ("M-Z" . zop-to-char)))
