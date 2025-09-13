;; init-ai.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
   :models '(deepseek-chat deepseek-coder))))


;; (use-package tabnine
;;  :custom
;;  (tabnine-wait 1)
;;  (tabnine-minimum-prefix-length 0)
;;  ;; (tabnine-executable-args (list "--log-level" "Error" "--no-lsp" "false"))
;;  :hook
;;  (on-first-input . tabnine-start-process)
;;  (prog-mode . tabnine-mode)
;;  (text-mode . tabnine-mode)
;;  (kill-emacs . tabnine-kill-process)
;;  :config
;;  (define-key tabnine-completion-map [tab] nil)
;;  (define-key tabnine-completion-map (kbd "M-f") #'tabnine-accept-completion-by-word)
;;  (define-key tabnine-completion-map (kbd "M-<return>") #'tabnine-accept-completion-by-line)
;;  (define-key tabnine-completion-map (kbd "C-g") #'tabnine-clear-overlay)
;;  (define-key tabnine-completion-map (kbd "M-[") #'tabnine-next-completion)
;;  (define-key tabnine-completion-map (kbd "M-]") #'tabnine-previous-completion)
;;  )
;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
;; (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
;;   (let ((company-message-func (ad-get-arg 0)))
;;     (when (and company-message-func
;;                (stringp (funcall company-message-func)))
;;       (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
;;         ad-do-it))))

;; (use-package minuet
;; :straight (minuet
;;	   :type git
;;	   :host github
;;	   :repo "emacsmirror/minuet")
;;  :bind
;;  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
;;   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
;;   ("C-c m" . #'minuet-configure-provider)
;;   :map minuet-active-mode-map
;;   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
;;   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
;;   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
;;   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
;;   ;; Accept the first line of completion, or N lines with a numeric-prefix:
;;   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
;;   ("M-a" . #'minuet-accept-suggestion-line)
;;   ("M-e" . #'minuet-dismiss-suggestion))

;;  :init
;;  ;; if you want to enable auto suggestion.
;;  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
;;  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

;;  :config
;;  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
;;  (setq minuet-provider 'openai-fim-compatible)

;;  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

;; For Evil users: When defining `minuet-ative-mode-map` in insert
;; or normal states, the following one-liner is required.

;; (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

;; This is *not* necessary when defining `minuet-active-mode-map`.

;; To minimize frequent overhead, it is recommended to avoid adding
;; `evil-normalize-keymaps` to `minuet-active-mode-hook`. Instead,
;; bind keybindings directly within `minuet-active-mode-map` using
;; standard Emacs key sequences, such as `M-xxx`. This approach should
;; not conflict with Evil's keybindings, as Evil primarily avoids
;; using `M-xxx` bindings.

(provide 'init-ai)
