;; init-corfu.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Bruce Wong

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

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
;; Auto-completion configurations.
;;
;;; Code:

(use-package corfu
 :custom
 (corfu-auto t)
 (corfu-cycle t)
 (corfu-auto-delay 0.2)
 (corfu-popupinfo-delay '(0.4 . 0.2))
 (corfu-auto-prefix 1)
 (corfu-preview-current nil)
 (corfu-echo-documentation t)
 (completion-ignore-case t)
 (read-file-name-completion-ignore-case t)
 (read-buffer-completion-ignore-case t)
 :bind
 ("M-/" . completion-at-point)
 (:map corfu-map
      ("TAB"         . corfu-next)
      ("C-n"         . corfu-next)
      ("C-p"         . corfu-previous)
      ("C-j"         . corfu-next)
      ("C-k"         . corfu-previous)
      ([tab]         . corfu-next)
      ([backtab]     . corfu-previous)
      ("<escape>"    . corfu-quit)
      ("S-TAB"       . corfu-previous))
 :hook ((after-init . global-corfu-mode)
	 (global-corfu-mode . corfu-popupinfo-mode))
  :config
    ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  )

;; Add extensions
(use-package cape
 ;; Bind dedicated completion commands
 ;; Alternative prefix keys: C-c p, M-p, M-+, ...
 ;; :bind (("C-c p p" . completion-at-point) ;; capf
 ;;  ("C-c p t" . complete-tag)        ;; etags
 ;;  ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
 ;;  ("C-c p h" . cape-history)
 ;;  ("C-c p f" . cape-file)
 ;;  ("C-c p k" . cape-keyword)
 ;;  ("C-c p s" . cape-elisp-symbol)
 ;;  ("C-c p e" . cape-elisp-block)
 ;;  ("C-c p a" . cape-abbrev)
 ;;  ("C-c p l" . cape-line)
 ;;  ("C-c p w" . cape-dict)
 ;;  ("C-c p :" . cape-emoji)
 ;;  ("C-c p \\" . cape-tex)
 ;;  ("C-c p _" . cape-tex)
 ;;  ("C-c p ^" . cape-tex)
 ;;  ("C-c p &" . cape-sgml)
 ;;  ("C-c p r" . cape-rfc1345))
 :init
 ;; Add to the global default value of `completion-at-point-functions' which is
 ;; used by `completion-at-point'.  The order of the functions matters, the
 ;; first function returning a result wins.  Note that the list of buffer-local
 ;; completion functions takes precedence over the global list.
;;  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
 (add-to-list 'completion-at-point-functions #'cape-file)
 (add-to-list 'completion-at-point-functions #'cape-elisp-block)
 ;;(add-to-list 'completion-at-point-functions #'cape-history)
 (add-to-list 'completion-at-point-functions #'cape-keyword)
;;  (add-to-list 'completion-at-point-functions #'cape-abbrev)
 ;;(add-to-list 'completion-at-point-functions #'cape-tex)
 ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
 ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
 ;;(add-to-list 'completion-at-point-functions #'cape-dict)
 ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
 ;;(add-to-list 'completion-at-point-functions #'cape-line)
 )
;; (use-package yasnippet-capf
;;  :after cape
;;  :config
;;  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
;;  (setq yasnippet-capf-lookup-by 'name) ;; Prefer the name of the snippet instead
;;  )

(use-package nerd-icons-corfu
 :straight (nerd-icons-corfu
	 :type git
	 :host github
	 :repo "LuigiPiucco/nerd-icons-corfu")
 :after corfu
 :config
 (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))




(provide 'init-corfu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ; init-corfu.el ends here
