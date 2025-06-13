;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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

(use-package dap-mode
  :diminish
  :bind (:map lsp-mode-map
	  ("<f5>" . dap-debug)
	  ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
	 (dap-mode . dap-ui-mode)
	 (dap-session-created . (lambda (_args) (dap-hydra)))
	 (dap-stopped . (lambda (_args) (dap-hydra)))
	 (dap-terminated . (lambda (_args) (dap-hydra/nil)))

	 (python-mode . (lambda () (require 'dap-python)))
	 (ruby-mode . (lambda () (require 'dap-ruby)))
	 (go-mode . (lambda () (require 'dap-go)))
	 (java-mode . (lambda () (require 'dap-java)))
	 ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
	 (php-mode . (lambda () (require 'dap-php)))
	 (elixir-mode . (lambda () (require 'dap-elixir)))
	 ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
	 (powershell-mode . (lambda () (require 'dap-pwsh)))))


;; Enable LSP in org babel
;; https://github.com/emacs-lsp/lsp-mode/issues/377
;; (setq centaur-lsp 'lsp-mode)
;; (cl-defmacro lsp-org-babel-enable (lang)
;;     "Support LANG in org source code block."
;;     (cl-check-type lang string)
;;     (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
;;            (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
;;       `(progn
;;          (defun ,intern-pre (info)
;;            (setq buffer-file-name (or (->> info caddr (alist-get :file))
;;                                       "org-src-babel.tmp"))
;;            (pcase centaur-lsp
;;              ('eglot
;;               (when (fboundp 'eglot-ensure)
;;                 (eglot-ensure)))
;;              ('lsp-mode
;;               (when (fboundp 'lsp-deferred)
;;                 ;; Avoid headerline conflicts
;;                 (setq-local lsp-headerline-breadcrumb-enable nil)
;;                 (lsp-deferred)))
;;              (_
;;               (user-error "LSP:: invalid `centaur-lsp' type"))))
;;          (put ',intern-pre 'function-documentation
;;               (format "Enable `%s' in the buffer of org source block (%s)."
;;                       centaur-lsp (upcase ,lang)))

;;          (if (fboundp ',edit-pre)
;;              (advice-add ',edit-pre :after ',intern-pre)
;;            (progn
;;              (defun ,edit-pre (info)
;;                (,intern-pre info))
;;              (put ',edit-pre 'function-documentation
;;                   (format "Prepare local buffer environment for org source block (%s)."
;;                           (upcase ,lang))))))))

;; (defvar org-babel-lang-list
;;   '("go" "python" "js" "css" "c" "rust" "cpp" "c++" "shell"))
;; (dolist (lang org-babel-lang-list)
;;   (eval `(lsp-org-babel-enable ,lang)))



(use-package lsp-mode
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :commands (lsp-format-buffer lsp-organize-imports)
  :hook ((prog-mode . (lambda ()
			(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
			  (lsp-deferred))))
	 ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
	 (lsp-mode . (lambda ()
		       ;; Integrate `which-key'
		       (lsp-enable-which-key-integration)

		       ;; Format and organize imports
		       (add-hook 'before-save-hook #'lsp-format-buffer t t)
		       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map lsp-mode-map
	  ("C-c C-d" . lsp-describe-thing-at-point)
	  ([remap xref-find-definitions] . lsp-find-definition)
	  ([remap xref-find-references] . lsp-find-references))
  :init (setq lsp-keymap-prefix "C-c l"
	      lsp-keep-workspace-alive nil
	      lsp-signature-auto-activate nil
	      lsp-modeline-code-actions-enable nil
	      lsp-modeline-diagnostics-enable nil
	      lsp-modeline-workspace-status-enable nil

	      lsp-semantic-tokens-enable t
	      lsp-inlay-hint-enable t
	      lsp-progress-spinner-type 'progress-bar-filled

		  lsp-headerline-breadcrumb-enable nil

	      lsp-enable-file-watchers nil
	      lsp-enable-folding nil
	      lsp-enable-symbol-highlighting nil
	      lsp-enable-text-document-color nil

	      lsp-enable-indentation t
	      lsp-enable-on-type-formatting nil

	      ;; For diagnostics
	      lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

	      ;; For clients
	      lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  :config
  ;; tabnine配合工作
  ;; (when (fboundp #'tabnine-completion-at-point)
  ;; (add-hook 'lsp-completion-mode-hook
  ;;           (defun lsp-capf ()
  ;;             (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t)
  ;;             (add-hook 'completion-at-point-functions
  ;;                       (cape-capf-super
  ;;                        #'lsp-completion-at-point
  ;;                        #'tabnine-completion-at-point) nil t))))

  (use-package consult-lsp
    :bind (:map lsp-mode-map
	    ("C-M-." . consult-lsp-symbols))))



(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "nf-fa-rocket" :face 'nerd-icons-green)
	   :color amaranth :quit-key ("q" "C-g"))
   ("Doc"
    (("d e" (progn
	      (lsp-ui-doc-enable (not lsp-ui-doc-mode))
	      (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
      "header" :toggle lsp-ui-doc-header)
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
	      (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
	      (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c u" . lsp-ui-imenu)
	 :map lsp-ui-mode-map
	 ("M-<f6>" . lsp-ui-hydra/body)
	 ("s-<return>" . lsp-ui-sideline-apply-code-actions)
	 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	 ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-delay 0.1
	lsp-ui-doc-show-with-cursor (not (display-graphic-p))
	lsp-ui-imenu-auto-refresh 'after-save
	lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
			      ,(face-foreground 'font-lock-string-face)
			      ,(face-foreground 'font-lock-constant-face)
			      ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
	  (if (facep 'posframe-border)
	      (face-background 'posframe-border nil t)
	    (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
	  (-let* ((win-width (frame-width))
		  (lsp-ui-peek-list-width (/ (frame-width) 2))
		  (string (-some--> (-zip-fill "" src1 src2)
			    (--map (lsp-ui-peek--adjust win-width it) it)
			    (-map-indexed 'lsp-ui-peek--make-line it)
			    (-concat it (lsp-ui-peek--make-footer)))))
	    (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
	    (posframe-show lsp-ui-peek--buffer
			   :string (mapconcat 'identity string "")
			   :min-width (frame-width)
			   :internal-border-color (face-background 'posframe-border nil t)
			   :internal-border-width 1
			   :poshandler #'posframe-poshandler-frame-center))
	(funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
	  (progn
	    (when (bufferp lsp-ui-peek--buffer)
	      (posframe-hide lsp-ui-peek--buffer))
	    (setq lsp-ui-peek--last-xref nil))
	(funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
	(goto-char 1)
	(while (setq next (next-single-property-change (or next 1) 'markdown-hr))
	  (when (get-text-property next 'markdown-hr)
	    (goto-char next)
	    (setq bolp (bolp)
		  before (char-before))
	    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
	    (setq after (char-after (1+ (point))))
	    (insert
	     (concat
	      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
	      (propertize "\n" 'face '(:height 0.5))
	      (propertize " "
			  ;; :align-to is added with lsp-ui-doc--fix-hr-props
			  'display '(space :height (1))
			  'lsp-ui-doc--replace-hr t
			  'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
	      ;; :align-to is added here too
	      (propertize " " 'display '(space :height (1)))
	      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))

;; `lsp-mode' and `treemacs' integration
(use-package lsp-treemacs
  :after lsp-mode
  :bind (:map lsp-mode-map
	  ("C-<f8>" . lsp-treemacs-errors-list)
	  ("M-<f8>" . lsp-treemacs-symbols)
	  ("s-<f8>" . lsp-treemacs-java-deps-list))
  :init (lsp-treemacs-sync-mode 1)
  :config
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (push 'lsp-treemacs-symbols-mode aw-ignored-buffers)
      (push 'lsp-treemacs-java-deps-mode aw-ignored-buffers))))


;; C/C++/Objective-C
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
  :config
  (with-no-warnings
    ;; FIXME: fail to call ccls.xref
    ;; @see https://github.com/emacs-lsp/emacs-ccls/issues/109
    (cl-defmethod my-lsp-execute-command
      ((_server (eql ccls)) (command (eql ccls.xref)) arguments)
      (when-let ((xrefs (lsp--locations-to-xref-items
			 (lsp--send-execute-command (symbol-name command) arguments))))
	(xref--show-xrefs xrefs nil)))
    (advice-add #'lsp-execute-command :override #'my-lsp-execute-command)))


(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
