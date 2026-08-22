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
;;			   "dz uncompress" "7z" (filename) nil t ""])

;; (add-to-list 'auto-mode-alist '("\\.dz\\'" nil jka-compr))

;; (add-to-list 'file-name-handler-alist '("\\.dz\\'" . jka-compr-handler))

(use-package lsp-bridge
 :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			  :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			  :build (:not compile))
 :config
 (global-lsp-bridge-mode)

 ;; Windows 下显式指定 Python（默认自动找 python.exe，通常无需设置）
 (cond
  ((eq system-type 'windows-nt)
    (setq lsp-bridge-python-command
          (expand-file-name "~/.emacs.d/.venv/Scripts/python.exe")))
  ((eq system-type 'gnu/linux)
    (setq lsp-bridge-python-command
          (expand-file-name "~/.emacs.d/.venv/bin/python3")))
  (t
    (setq lsp-bridge-python-command "python3")))  ; 默认回退

 ;; 补全菜单外观
 (setq acm-enable-icon t)
 (setq acm-enable-doc t)
 (setq acm-enable-doc-markdown-render 'async)
 (setq acm-enable-quick-access t)

  ;; 非 LSP 补全：启用 capf 后端，平替原 corfu+cape 的 file/keyword 补全
  (setq acm-enable-capf t)

  ;; Jupyter 内核补全（需 melpa jupyter 包，配合 org-mode ob-jupyter 源码块）
  (setq acm-enable-jupyter t)

  ;; org-roam 笔记链接/标题补全（需 org-roam 包）
  (setq acm-enable-org-roam t)

  ;; 关闭 tabnine（默认开启，需另行安装且吃 CPU）
  (setq acm-enable-tabnine nil)

  ;; 参考原 corfu 的候选切换快捷键（evil 友好）
  ;; acm 已通过 remap next-line/previous-line 支持 C-n/C-p
  (with-eval-after-load 'acm
    (define-key acm-mode-map (kbd "C-j") #'acm-select-next)   ; 向下切下一个候选
    (define-key acm-mode-map (kbd "C-k") #'acm-select-prev)   ; 向上切上一个候选
    (define-key acm-mode-map (kbd "S-TAB") #'acm-select-prev) ; Shift-TAB 向上
    (define-key acm-mode-map (kbd "<backtab>") #'acm-select-prev))

  ;; evil insert state 里 C-k 默认绑定 evil-insert-digraph，优先级高于 acm-mode-map，
  ;; 导致补全菜单里按 C-k 无法向上切换（还会进入 digraph 输入、打印问号）。
  ;; 这里覆盖 C-k：菜单激活时向上切换，否则回退 digraph 原行为。
  ;; 注意：命令名必须以 acm- 开头，否则 acm--pre-command 会把菜单隐藏
  ;;（acm-continue-commands 用 "\\`acm-" 正则放行补全菜单内的命令）。
  (defun acm-select-prev-or-digraph ()
    (interactive)
    (if (and (boundp 'acm-mode) acm-mode)
        (acm-select-prev)
      (evil-insert-digraph)))

  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-k") #'acm-select-prev-or-digraph))

  ;; 让纯文本等非 LSP 模式也有补全（org/markdown 已在默认列表内）
 (dolist (hook '(text-mode-hook))
   (add-to-list 'lsp-bridge-default-mode-hooks hook)))

;; capf 后端（由 acm-enable-capf 调用），平替原 corfu+cape 的非 LSP 补全
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'init-lsp-bridge)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
