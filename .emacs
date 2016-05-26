;; Copyright (C) 2016 Ross A. Baker
;; 
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.



;; Inspirations:
;; * https://github.com/lunaryorn/.emacs.d
;; * https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
;; * https://github.com/technomancy/better-defaults/blob/master/better-defaults.el



;;; Packaging

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



;;; Better defaults

(setq-default
 confirm-kill-emacs 'yes-or-no-p
 cursor-in-non-selected-windows nil
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-scratch-message ""
 load-prefer-newer t
 mouse-yank-at-point t
 tab-width 4
 uniquify-buffer-name-style 'forward
 x-select-enable-clipboard t
 x-select-enable-primary t)

;;; Appearance

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (use-package ample-theme
    :ensure t
    :init
    (load-theme 'ample t t)
    (load-theme 'ample-flat t t)
    (load-theme 'ample-light t t)
    (enable-theme 'ample-flat)))

(blink-cursor-mode -1)
(show-paren-mode 1)

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(fset 'yes-or-no-p #'y-or-n-p)

(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  :diminish anzu-mode)

(use-package which-func
  :ensure t
  :init (which-function-mode))

(use-package ibuffer
  :ensure t
  :bind (([remap list-buffers] . ibuffer)))

(use-package ibuffer-vc
  :ensure t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package server
  :init (server-mode))

(use-package edit-server
  :ensure t
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

(use-package dired
  :config
  (setq dired-auto-revert-buffer t)
  (when
      (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package recentf
  :init (recentf-mode))

(setq view-read-only t)

;;; Lines and columns and such

(use-package linum-off
  :ensure t
  :init (global-linum-mode 1))
(setq column-number-mode t)
(add-hook 'prog-mode-hook (lambda () (set-fill-column 80)))

;; https://github.com/alpaker/Fill-Column-Indicator/issues/21
;; https://github.com/purcell/emacs.d/blob/d02323adcdea7f00ad26bc308bf06ce8d1eefb3b/lisp/init-editing-utils.el#L198-L230
(use-package fill-column-indicator
  :ensure t
  :init
  (defun rossabaker/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))
  
  (add-hook 'prog-mode-hook 'rossabaker/prog-mode-fci-settings)

  (defun rossabaker/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar rossabaker/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (rossabaker/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'rossabaker/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and rossabaker/fci-mode-suppressed
               (null popup-instances))
      (setq rossabaker/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (rossabaker/fci-enabled-p)
          (turn-on-fci-mode))))))

;;; Backup behavior

(setq rossabaker/backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p rossabaker/backup-directory))
    (make-directory rossabaker/backup-directory))
(setq backup-directory-alist `(("" . ,rossabaker/backup-directory))
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

;;; Version control

(use-package diff-hl
  :config
  (require 'diff-hl-flydiff)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;;; Basic programming config

(defun rossabaker/desperately-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
  (with-temp-buffer
    (cd (locate-dominating-file default-directory "Makefile"))
    (compile "make -k"))))

;;; Scala

(use-package ensime
  :commands ensime ensime-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(defun rossabaker/ensime-project-p ()
  "Are we in an ensime project?"
  (and (buffer-file-name) (ensime-config-find-file (buffer-file-name))))
(defun rossabaker/maybe-compile-with-ensime ()
  "Compile with Ensime where appropriate, compile where not"
  (interactive)
  (if (rossabaker/ensime-project-p)
      (ensime-sbt-do-compile)
    (call-interactively 'rossabaker/desperately-compile)))

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

;;; Ido

(use-package ido
  :ensure t
  :init
  (ido-mode t)
  (ido-everywhere t)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))
(use-package ido-vertical-mode
  :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-vertical-mode)))
(use-package ido-sort-mtime
  :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-sort-mtime-mode 1)))  

;;; Miscellaneous

(use-package projectile
  :ensure t
  :config (projectile-global-mode))

(use-package magit
  :ensure t)

;;; Keymaps

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c c") 'rossabaker/maybe-compile-with-ensime)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-X C-z"))
(global-unset-key (kbd "C-z"))
