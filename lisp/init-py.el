;; init-py.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2024 Haibo Wang

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

(use-package lsp-pyright
  :preface
  ;; Use yapf to format
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (((python-mode python-ts-mode) . (lambda ()
					   (require 'lsp-pyright)
					   (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t))))
  :init (when (executable-find "python3")
	  (setq lsp-pyright-python-executable-cmd "python3")))

;; (use-package python
;;   :ensure nil
;;   :hook (inferior-python-mode . (lambda ()
;;				  (process-query-on-exit-flag
;;				   (get-process "Python"))))
;;   :init
;;   ;; Disable readline based native completion
;;   (setq python-shell-completion-native-enable nil)
;;   :config
;;   ;; Default to Python 3. Prefer the versioned Python binaries since some
;;   ;; systems stupidly make the unversioned one point at Python 2.
;;   (when (and (executable-find "python3")
;;	     (string= python-shell-interpreter "python"))
;;     (setq python-shell-interpreter "python3"))
;; Env vars
;; (with-eval-after-load 'exec-path-from-shell
;;   (exec-path-from-shell-copy-env "PYTHONPATH")))

(provide 'init-py)
