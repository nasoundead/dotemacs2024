;; init-lsp-bridge.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Haibo Wang

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
;; Golang configurations.
;;

;;; Code:
;;
;;

;; (defadvice jka-compr-info-compress-args (around eval-args activate)
;;   "Evaluate program arguments"
;;   (setq ad-return-value (mapcar 'eval (aref info 3))))

;; (defadvice jka-compr-info-uncompress-args (around eval-args activate)
;;   "Evaluate program arguments"
;;   (setq ad-return-value (mapcar 'eval (aref info 6))))


;; (add-to-list 'jka-compr-compression-info-list ["\\.dz\\'" "7z" "7z" ("-")
;; 			   "dz uncompress" "7z" (filename) nil t ""])

;; (add-to-list 'auto-mode-alist '("\\.dz\\'" nil jka-compr))

;; (add-to-list 'file-name-handler-alist '("\\.dz\\'" . jka-compr-handler))

(use-package lsp-bridge
 :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			  :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			  :build (:not compile))
 :config
 (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
