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

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(if (file-readable-p custom-file)
    (load custom-file))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish)

;; h/t https://github.com/bodil/emacs.d
;;
;; When I was a child, I spake as a child,
;; I understood as a child, I thought as a child:
;; but when I became a man, I put away childish things.
;;   -- 1 Corinthians, 13:11
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-message t
      initial-scratch-message nil)

(load-file (expand-file-name "~/.emacs.d/fira.el"))
(use-package material-theme
  :config
  (load-theme 'material t))

;; h/t https://www.emacswiki.org/emacs/AlarmBell
(defun ross/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell       nil
      ring-bell-function #'ross/terminal-visible-bell)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package magit
  :bind ("C-x g" . magit-status))

(setq show-trailing-whitespace t
      indent-tabs-mode nil
      require-file-newline t)
;; https://github.com/r0man/.emacs.d/blob/master/init.el.org
(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(defun untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))
(defun cleanup-buffer ()
  "Cleanup the current buffer."
  (interactive)
  (indent-buffer)
  (delete-trailing-whitespace))
;; Trim trailing whitespace on write, from modified lines only
(use-package ws-butler
  :diminish ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package server
  :init (server-start))

(use-package edit-server
  :config
  (setq edit-server-new-frame nil)
  :init
  (edit-server-start))

;; Must happen after exec-path-from-shell, else https://github.com/ensime/ensime-server/issues/670
(use-package ensime
  :config
  (setq scala-indent:use-javadoc-style t))
(defun ross/ensime-project-p ()
  "Are we in an Ensime project?"
  (ensime-config-find-file (or buffer-file-name default-directory)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-load-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-load-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package lorem-ipsum)

(use-package markdown-mode)

(use-package hcl-mode)

(use-package web-mode
  :config
  (setq web-mode-engines-alist
	'(("php"    . "\\.phtml\\'")
	  ("blade"  . "\\.blade\\.")
	  ("go"     . "\\.ctmpl\\'"))))

(use-package unfill
  :bind
  ("M-q" . unfill-toggle))

(use-package eshell
  :config
  ;; This causes an infinite loop with Fira Code
  (setq eshell-status-in-modeline nil))

(use-package guru-mode
  :diminish guru-mode
  :config
  (guru-global-mode +1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c SPC") 'just-one-space) ; work around Spotlight conflict on OSX

(defun compile-smarter ()
  "Compile with ensime-sbt-do-compile if in an Ensime project, else with compile"
  (interactive)
  (call-interactively
   (if (ross/ensime-project-p)
       'ensime-sbt-do-compile
     'compile)))
(global-set-key (kbd "C-c c") 'compile-smarter)

(let ((docker-file (expand-file-name "~/.emacs.d/custom.el")))
  (if (file-readable-p docker-file)
    (load docker-file)))

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta))
