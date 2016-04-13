;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Appearance

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'sanityinc-tomorrow-night t))

(show-paren-mode 1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;;; emacsclient

(server-start)

;;; Lines and columns and such

(global-linum-mode 1)
(require 'linum-off)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook (lambda () (set-fill-column 80)))

;;; Version control

(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;;; Basic programming config

(setq-default indent-tabs-mode nil)

;;; Scala

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(defun rossabaker/ensime-project-p ()
  "Are we in an ensime project?"
  (and (buffer-file-name) (ensime-config-find-file (buffer-file-name))))
(defun rossabaker/maybe-compile-with-ensime ()
  "Compile with Ensime where appropriate, compile where not"
  (interactive)
  (if (rossabaker/ensime-project-p)
      (ensime-sbt-do-compile)
    (call-interactively 'compile)))

;;; Go

(defun rossabaker/go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go vet")))
(add-hook 'go-mode-hook 'rossabaker/go-mode-hook)

;;; Markdown

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Chrome integration

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; Miscellaneous

(projectile-global-mode)
(ido-mode t)

;;; Keymaps

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c c") 'rossabaker/compile)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x g") 'magit-status)

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-X C-z"))
(global-unset-key (kbd "C-z"))
