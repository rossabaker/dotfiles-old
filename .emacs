;; -*- mode: Emacs-Lisp; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(server-start)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(load-theme 'sanityinc-tomorrow-night t)

(setq-default indent-tabs-mode nil)

(require 'linum-off)
(global-linum-mode 1)

(setq column-number-mode t)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook (lambda () (set-fill-column 80)))

(ido-mode t)
(projectile-global-mode)

(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(defun rossabaker/ensime-project-p ()
  "Are we in an ensime project?"
  (and (buffer-file-name) (ensime-config-find-file (buffer-file-name))))

(defun rossabaker/compile ()
  "Compile with Ensime where appropriate, compile where not"
  (interactive)
  (if (rossabaker/ensime-project-p)
      (ensime-sbt-do-compile)
    (call-interactively 'compile)))

(defun rossabaker/go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go vet")))
(add-hook 'go-mode-hook 'rossabaker/go-mode-hook)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-c c") 'rossabaker/compile)

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-X C-z"))
(global-unset-key (kbd "C-z"))