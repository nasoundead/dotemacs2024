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
  ;; 强制 gptel 请求/响应全程 UTF-8
  (setq gptel-coding-system 'utf-8)
  (setq gptel-response-coding-system 'utf-8)

  ;; 先检查原函数是否存在，避免覆盖出错
  (when (fboundp 'gptel-curl--get-args)
    ;; 移除可能存在的旧 advice，防止重复覆盖
    (advice-remove 'gptel-curl--get-args #'gptel-curl--get-args)

    ;; 重新定义修复函数（改名避免和原函数重名）
    (defun my-gptel-curl--get-args (orig-func info token)
      "Advice for gptel-curl--get-args to force UTF-8 on Windows (不干扰补全)."
      (let* ((data (plist-get info :data))
	     ;; 强制用纯 UTF-8 编码 JSON 数据，避免 dos 格式干扰
	     (data-json (encode-coding-string (gptel--json-encode data) 'utf-8 t))
	     (url (plist-get info :url))
	     (headers (plist-get info :headers))
	     (args (list "-s" "-S" "-X" "POST"
			 "-H" "Content-Type: application/json; charset=utf-8"
			 ;; 额外加 Accept 编码头，要求接口返回纯 UTF-8
			 "-H" "Accept: application/json; charset=utf-8")))
	(dolist (header headers)
	  (setq args (append args (list "-H" header))))
	(setq args (append args (list "-d" data-json url)))
	args))

    ;; 使用 :around 类型的 advice（更安全，不直接覆盖原函数）
    (advice-add 'gptel-curl--get-args :around #'my-gptel-curl--get-args))

  ;; 兜底：确保 gptel 缓冲区显示编码为 UTF-8
  (add-hook 'gptel-after-response-hook
	    (lambda ()
	      (set-buffer-process-coding-system 'utf-8 'utf-8)
	      (set-buffer-file-coding-system 'utf-8)))

  (setq gptel-model   'deepseek-chat
	gptel-backend
	(gptel-make-openai "DeepSeek"     ;Any name you want
	  :host "api.deepseek.com"
	  :endpoint "/chat/completions"
	  :stream t
	  :key (lambda () (getenv "DEEPSEEK_API_KEY"))  ; 读取环境变量
	  :models '(deepseek-chat deepseek-coder)))
  (defun get-ollama-models ()
    "Fetch the list of installed Ollama models."
    (let* ((output (shell-command-to-string "ollama list"))
	   (lines (split-string output "\n" t))
	   models)
      (dolist (line (cdr lines))  ; Skip the first line
	(when (string-match "^\\([^[:space:]]+\\)" line)
	  (push (match-string 1 line) models)))
      (nreverse models)))

  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models (get-ollama-models))          ;List of models
  )


;; (use-package ollama-buddy
;;   :ensure t
;;   :straight (ollama-buddy
;;	     :type git
;;	     :host github
;;	     :repo "captainflasmr/ollama-buddy")
;;   :bind
;;   ("C-c o" . ollama-buddy-role-transient-menu)
;;   ("C-c O" . ollama-buddy-transient-menu))

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
