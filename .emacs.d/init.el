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
(add-to-list 'package-archives
      '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(if (file-readable-p custom-file)
    (load custom-file))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'bind-key)

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

(when (memq window-system '(ns mac))
  (set-default-font "Fira Code")
  (mac-auto-operator-composition-mode))

;; h/t https://www.emacswiki.org/emacs/AlarmBell
(defun ross/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell       nil
      ring-bell-function #'ross/terminal-visible-bell)

(use-package magit
  :bind ("C-x g" . magit-status))

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
;; Trim trailing whitespace on write, from modified lines only
(use-package ws-butler
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))
