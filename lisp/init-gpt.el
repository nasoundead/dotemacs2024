;; init-gpt.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
(use-package gptel
  :config
  (setq gptel-response-coding-system 'utf-8)
  ;; OPTIONAL configuration
  (setq gptel-model   'deepseek-chat
	gptel-backend
	(gptel-make-openai "DeepSeek"     ;Any name you want
	  :host "api.deepseek.com"
	  :endpoint "/chat/completions"
	  :stream t
	  :key "sk-ac5fd897d2b8440694b77a72fcef9bde"             ;can be a function that returns the key
	  :models '(deepseek-chat deepseek-coder)))

  )


(provide 'init-gpt)
