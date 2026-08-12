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
  (setq gptel-coding-system 'utf-8)
  (setq gptel-response-coding-system 'utf-8)
  (let* ((host  (or (getenv "AI_HOST")  "api.deepseek.com"))
	 (path  (or (getenv "AI_PATH")  "/chat/completions"))
	 (model (intern (or (getenv "AI_MODEL") "deepseek-chat")))
	 (key   (or (getenv "AI_API_KEY") (getenv "DEEPSEEK_API_KEY"))))
    (setq gptel-api-key key)
    (setq gptel-backend
	  (gptel-make-openai "LLM"
	    :host host
	    :endpoint path
	    :stream t
	    :key gptel-api-key
	    :models (list model)))
    (setq gptel-model model))

  ;; 自动把Markdown格式转为Org原生格式 (默认已开启)
  ;; 设置默认系统提示词 (替代旧版已删除的 gptel-default-system-prompt)
  (setf (alist-get 'default gptel-directives)
	"输出纯文本Org-mode内容，禁止使用任何标记语法（不要用*加粗*、/斜体/、=代码=、~原样~），正文直接写。标题用*层级。列表用-短横线。")
)

;; ;; Org-AI: AI 辅助写作，深度集成 org-mode
;; (use-package websocket
;;   :straight t)

;; (use-package org-ai
;;   :straight (org-ai :type git :host github :repo "rksm/org-ai")
;;   :defer t
;;   :after org
;;   :init
;;   ;; 确保编码
;;   (setq org-ai-coding-system 'utf-8)
;;   (add-hook 'org-mode-hook #'org-ai-mode)
;;   (org-ai-global-mode 1)
;;   :config
;;   ;; --- LLM 配置 (通过环境变量适配不同机器/不同模型) ---
;;   ;; AI_HOST、AI_MODEL、AI_API_KEY 与 init-fanyi.el 共用同一组变量
;;   (let* ((host  (or (getenv "AI_HOST")  "https://api.deepseek.com"))
;; 	 (model (or (getenv "AI_MODEL") "deepseek-chat"))
;; 	 (key   (or (getenv "AI_API_KEY") (getenv "DEEPSEEK_API_KEY"))))
;;     (setq org-ai-service
;; 	  (cond ((string-match-p "deepseek" host) 'deepseek)
;; 		((string-match-p "openai" host)   'openai)
;; 		(t                                'openai)))
;;     (setq org-ai-default-chat-model model)
;;     (add-to-list 'org-ai-chat-models model)
;;     (setq org-ai-openai-api-token key))
;;   (setq org-ai-default-chat-system-prompt
;; 	"你需要使用Org-mode原生格式输出内容：标题用*层级标记、无序列表用短横-、代码块用#+begin_src包裹，禁止Markdown的#标题、```代码围栏符号。")
;;   ;; yasnippet 集成
;;   (org-ai-install-yasnippets))

;; ── TOEIC 学习 ──
(defun sea/toeic-slug (title)
  "Convert TITLE to a file-name-safe slug."
  (replace-regexp-in-string
   " " "-" (replace-regexp-in-string "[^[:alnum:] ]" "" (downcase title))))

(defun sea/toeic-generate-article ()
       "调用 AI 生成一篇托业级英文文章，创建 org-roam 笔记。
含原文、阅读理解题、答案、词汇拆解、句型分析。"
       (interactive)
       (require 'org)
       (let ((repos (expand-file-name "straight/repos" user-emacs-directory)))
	    (dolist (pkg '("org-roam" "emacsql" "emacsql-sqlite-builtin"))
		    (let ((pkg-dir (expand-file-name pkg repos)))
			 (when (file-directory-p pkg-dir)
			       (add-to-list 'load-path pkg-dir)))))
       (require 'org-roam)
       (let* ((topic (read-string "文章主题 (可选，回车随机): "))
	      (prompt
	       (concat
		"Generate a TOEIC-level English article"
		(if (string-empty-p topic) ""
		    (format " about \"%s\"" topic))
		". Output a valid Org-mode document with these sections:\n\n"
		"* Original Article\n  The full article (300-500 words).\n\n"
		"* Reading Comprehension\n  5 multiple-choice questions formatted as:\n"
		"  Q1. question\n    A) ...  B) ...  C) ...  D) ...\n\n"
		"* Answers\n  Q1. A - brief explanation\n\n"
		"* Article Breakdown\n  Paragraph-by-paragraph Chinese translation.\n\n"
		"* Key Vocabulary\n  10-15 words in a table:\n"
		"  | Word | Definition | Example |\n\n"
		"* Key Phrases & Patterns\n  Important phrases and sentence patterns.\n\n"
		"* Word Roots & Derivatives\n"
		"  For 5-8 key words, show root, derivatives with examples.\n\n"
		"Output ONLY the Org content, no extra commentary."))
	      (title (or (and (not (string-empty-p topic)) topic) "TOEIC Article"))
	      (slug (sea/toeic-slug title))
	      (file-path (expand-file-name
			  (format "%s-toeic-%s.org" slug
				  (format-time-string "%Y%m%d%H%M%S"))
			  org-roam-directory))
	      (auto-title (string-empty-p topic))
	      (api-key (or (getenv "AI_API_KEY") (getenv "DEEPSEEK_API_KEY")))
	      (api-host (or (getenv "AI_HOST") "https://api.deepseek.com"))
	      (api-path (or (getenv "AI_PATH") "/chat/completions"))
	      (api-model (or (getenv "AI_MODEL") "deepseek-chat"))
	      (api-url (concat api-host
			       (if (string-suffix-p "/" api-host)
				   (substring api-path 1)
				 api-path))))
	      (unless api-key
		(user-error "请设置 AI_API_KEY 或 DEEPSEEK_API_KEY 环境变量"))
	      (message "正在请求 AI (%s) ..." api-model)
	      (let* ((url api-url)
		     (url-request-method "POST")
		     (url-request-extra-headers
		      `(("Content-Type" . "application/json")
			("Authorization" . ,(concat "Bearer " api-key))))
		     (url-request-data
		      (json-encode
		       `((model . ,api-model)
			 (messages . [((role . "user") (content . ,prompt))])
			 (stream . :json-false))))
		     (resp-buf (url-retrieve-synchronously url))
		     content)
		     (unless resp-buf
		       (user-error "网络请求失败，请检查网络连接"))
		     (with-current-buffer resp-buf
		       (goto-char (point-min))
		       (unless (re-search-forward "\n\n" nil t)
			       (kill-buffer)
			       (user-error "API 返回格式异常，可能是 API Key 无效"))
	       (condition-case err
		   (let* ((json (json-parse-buffer :object-type 'alist))
			  (choices (cdr (assoc 'choices json)))
			  (msg (cdr (assoc 'message (aref choices 0)))))
		     (setq content (cdr (assoc 'content msg))))
		 (error (kill-buffer)
			(user-error "AI 响应解析失败: %s" (error-message-string err))))
		       (kill-buffer))
		     (unless (and content (not (string-empty-p content)))
			     (user-error "AI 未返回内容，请检查网络和 API Key"))
		     (when auto-title
		       (when (string-match "\\* Original Article\n[ \t]*\\(.+\\)" content)
			     (let ((new-title (string-trim (match-string 1 content))))
				  (unless (string-empty-p new-title)
					  (setq title new-title)
					  (setq slug (sea/toeic-slug title))
					  (setq file-path (expand-file-name
							   (format "%s-toeic-%s.org" slug
								   (format-time-string "%Y%m%d%H%M%S"))
							   org-roam-directory))))))
		     (with-temp-buffer
		      (insert "#+title: " title "\n")
		      (insert "#+date: "
			      (let ((system-time-locale "C"))
				   (format-time-string "[%Y-%m-%d %a %H:%M]"))
			      "\n")
		      (insert "#+last_modified: \n\n")
		      (insert content)
		      (make-directory (file-name-directory file-path) t)
		      (write-region nil nil file-path nil 'silent))
		     (org-roam-db-sync)
		     (find-file file-path)
		     (visual-line-mode 1)
		     (message "TOEIC 笔记已创建: %s" file-path))))

(defun sea/toeic-open-index ()
  "打开或创建 TOEIC 学习索引 org 文件。"
  (interactive)
  (let ((idx (expand-file-name "toeic-index.org" org-roam-directory)))
    (unless (file-exists-p idx)
      (with-temp-buffer
	(insert "#+title: TOEIC 学习索引\n")
	(insert "#+date: "
		(let ((system-time-locale "C"))
		  (format-time-string "[%Y-%m-%d %a %H:%M]"))
		"\n\n")
	(insert "* 托业阅读练习\n")
	(write-region nil nil idx nil 'silent)))
    (find-file idx)
    (unless (derived-mode-p 'org-mode) (org-mode))))

;; ── ellama: LLM 补全 / 对话 ──
(use-package llm
  :straight t
  :init
  (setq llm-warn-on-nonfree nil))

;; (use-package ellama
;;   :straight t
;;   :after llm
;;   :init
;;   (setq ellama-auto-scroll t)
;;   (setq ellama-keymap-prefix "C-c e")
;;   :config
;;   (require 'llm-deepseek)
;;   (let* ((host (or (getenv "AI_HOST") "https://api.deepseek.com"))
;; 	 (model (or (getenv "AI_MODEL") "deepseek-chat"))
;; 	 (key (or (getenv "AI_API_KEY") (getenv "DEEPSEEK_API_KEY"))))
;;     (setq ellama-provider
;; 	  (if (string-match-p "deepseek" host)
;; 	      (make-llm-deepseek :key key :chat-model model)
;; 	    (make-llm-openai-compatible :url host :key key :chat-model model))))
;;   (ellama-context-header-line-global-mode +1))

(provide 'init-ai)
