;; init-font.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2025 Haibo Wang

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  configurations.
;;

;;; Code:
;;
;;
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)

(defvar sea-font-size 13
 "Current font size.")

(defvar sea-fonts '(
		    ;; (default . "Fira Code")
		    ;; (default . "Comic Code")
		    ;; (default . "Monaco Nerd Font")
		    ;; (default . "FantasqueSansM Nerd Font")
		    ;; (default . "FiraCode Nerd Font")
		    ;; (default . "Iosevka NF")
		    (default . "Iosevka Comfy")
		    (cjk . "Microsoft Yahei")
		    (symbol . "Symbola")
			(emoji . "Segoe UI Emoji")  ; Windows
			;; (emoji . "Apple Color Emoji")  ; macOS
			;; (emoji . "Noto Color Emoji")  ; Linux
		    (fixed . "Iosevka Comfy Fixed")
		    (fixed-serif . "Iosevka Comfy Motion")
		    (variable . "Iosevka Comfy Motion Duo")
		    (wide . "Iosevka Comfy Wide")
		    (tall . "Iosevka Comfy Motion"))
 ;; (tall . "Monospace"))
 "Fonts to use.")

(defun sea--get-font-family (key)
    (let ((font (alist-get key sea-fonts)))
	(if (string-empty-p font)
	    (alist-get 'default sea-fonts)
	font)))

(defun sea-load-default-font ()
    "Load default font configuration."
    (let ((default-font (format "%s-%s"
				(sea--get-font-family 'default)
				sea-font-size)))
	(add-to-list 'default-frame-alist (cons 'font default-font))))

(defun sea-load-face-font ()
"Load face font configuration."
(let ((variable-font (sea--get-font-family 'variable))
	(fixed-font (sea--get-font-family 'fixed))
	(fixed-serif-font (sea--get-font-family 'fixed-serif)))
    (set-face-attribute 'variable-pitch nil :family variable-font)
    (set-face-attribute 'fixed-pitch nil :family fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font)))

(defun sea-load-charset-font (&optional font)
    "Load charset font configuration with fallback."
    (let ((default-font (or font (format "%s-%s"
                                         (sea--get-font-family 'default)
                                         sea-font-size)))
          (cjk-font (sea--get-font-family 'cjk))
          (symbol-font (sea--get-font-family 'symbol))
          (emoji-font (sea--get-font-family 'emoji)))
    (set-frame-font default-font)
    (dolist (charset '(kana han hangul cjk-misc bopomofo))
        (set-fontset-font t charset cjk-font))
    (set-fontset-font t 'symbol symbol-font)
    
    ;; 设置 Emoji 字体，添加多个备选方案
    (set-fontset-font t 'unicode 
                      (cond 
                       ((x-list-fonts emoji-font) emoji-font)  ; 首选 Emoji 字体
                       ((x-list-fonts "Segoe UI Emoji") "Segoe UI Emoji")  ; Windows 备选
                       ((x-list-fonts "Apple Color Emoji") "Apple Color Emoji")  ; macOS 备选
                       ((x-list-fonts "Noto Color Emoji") "Noto Color Emoji")  ; Linux 备选
                       (t default-font))  ; 无匹配字体时使用默认
                      nil 'prepend)))

(sea-load-default-font)
(sea-load-face-font)

(defvar sea-font-current-variant nil)

(defun sea-dynamic-set-font (&rest ignore)
    (interactive)
    (when window-system
    (when (or (frame-root-window-p (get-buffer-window))
              (frame-root-window-p (window-parent)))
        (let* ((prev-font-style sea-font-current-variant)
               (wl (seq-filter (lambda (w) (not (string-prefix-p " " (buffer-name (window-buffer w))))) (window-list)))
               (def (sea--get-font-family 'default))
               (new-variant (cond
                             ((= 1 (length wl))
                              (sea--get-font-family 'default))
                             ((window-combined-p)
                              (sea--get-font-family 'tall))
                             (t
                              (sea--get-font-family 'wide)))))
        (unless (equal prev-font-style new-variant)
            (setq sea-font-current-variant new-variant)
            (set-frame-font new-variant)
            ;; 重新应用字符集字体设置，包括 Emoji
            (sea-load-charset-font new-variant))))))

;; (setq frame-inhibit-implied-resize t)
(add-hook 'window-state-change-hook 'sea-dynamic-set-font)

;; Run after startup
(add-hook 'after-init-hook
	  (lambda ()
	    (when window-system
	      (sea-load-charset-font))))


(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
				   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
				      all-the-icons-faicon all-the-icons-fileicon
				      all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty))
    )

(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)

(provide 'init-font)
