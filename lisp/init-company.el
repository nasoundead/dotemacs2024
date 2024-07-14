(use-package company-lsp)
;; (use-package company-tabnine)
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-async t)
  (setq company-backends
	'(
	  company-dabbrev-code
	  company-keywords
	  company-files
	  company-yasnippet
	  ;; Company-tabnine
	  company-lsp
	  ))
  (setq company-frontends
	'(company-pseudo-tooltip-unless-just-one-frontend
	  company-preview-frontend
	  company-echo-metadata-frontend))

  ;; (add-hook 'company-mode-hook #'(lambda ()
  ;;				   (setq company-backends
  ;;					 (delete 'company-bbdb
  ;;						 (delete 'company-oddmuse
  ;;							 (delete 'company-cmake
  ;;								 (delete 'company-clang company-backends)))))))

  (defun advice-only-show-tooltip-when-invoked (orig-fun command)
    "原始的 company-pseudo-tooltip-unless-just-one-frontend-with-delay, 它一直会显示
	candidates tooltip, 除非只有一个候选结果时，此时，它会不显示, 这个 advice 则是让其
	完全不显示, 但是同时仍旧保持 inline 提示, 类似于 auto-complete 当中, 设定
	ac-auto-show-menu 为 nil 的情形, 这种模式比较适合在 yasnippet 正在 expanding 时使用。"
    (when (company-explicit-action-p)
      (apply orig-fun command)))

  (defun advice-always-trigger-yas (orig-fun &rest command)
    (interactive)
    (unless (ignore-errors (yas-expand))
      (apply orig-fun command)))

  (with-eval-after-load 'yasnippet
    (defun yas/disable-company-tooltip ()
      (interactive)
      (advice-add #'company-pseudo-tooltip-unless-just-one-frontend :around #'advice-only-show-tooltip-when-invoked)
      (define-key company-active-map [tab] 'yas-next-field-or-maybe-expand)
      (define-key company-active-map (kbd "TAB") 'yas-next-field-or-maybe-expand)
      )
    (defun yas/restore-company-tooltip ()
      (interactive)
      (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend #'advice-only-show-tooltip-when-invoked)
      (set-company-tab)
      )
    (add-hook 'yas-before-expand-snippet-hook #'yas/disable-company-tooltip)
    (add-hook 'yas-after-exit-snippet-hook #'yas/restore-company-tooltip)

    ;; 这个可以确保，如果当前 key 是一个 snippet, 则一定展开 snippet,
    ;; 而忽略掉正常的 company 完成。
    (advice-add #'company-select-next-if-tooltip-visible-or-complete-selection :around #'advice-always-trigger-yas)
    (advice-add #'company-complete-common :around #'advice-always-trigger-yas)
    (advice-add #'company-complete-common-or-cycle :around #'advice-always-trigger-yas))


  (setq company-auto-commit t)
  ;; 32 空格, 41 右圆括号, 46 是 dot 字符
  ;; 这里我们移除空格，添加逗号(44), 分号(59)
  ;; 注意： C-x = 用来检测光标下字符的数字，(insert 数字) 用来测试数字对应的字符。
  (setq company-auto-commit-chars '(41 46 44 59))

  :bind
  ("M-/" . completion-at-point)
  (:map company-active-map
    ("TAB"    . company-select-next-if-tooltip-visible-or-complete-selection)
    ("S-TAB"  . company-select-previous)
    ([tab]    . company-select-next-if-tooltip-visible-or-complete-selection)
    ([backtab]    . company-select-previous)
    ("C-n"    . company-select-next-if-tooltip-visible-or-complete-selection)
    ("C-p"    . company-select-previous)
    ("C-j"    . company-select-next-if-tooltip-visible-or-complete-selection)
    ("C-k"    . company-select-previous)
    )
  :hook ((after-init . global-corfu-mode)))

(use-package tabnine
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 0)
  ;; (tabnine-executable-args (list "--log-level" "Error" "--no-lsp" "false"))
  :hook
  (on-first-input . tabnine-start-process)
  (prog-mode . tabnine-mode)
  (text-mode . tabnine-mode)
  (kill-emacs . tabnine-kill-process)
  :config
  (define-key tabnine-completion-map [tab] nil)
  (define-key tabnine-completion-map (kbd "M-f") #'tabnine-accept-completion-by-word)
  (define-key tabnine-completion-map (kbd "M-<return>") #'tabnine-accept-completion-by-line)
  (define-key tabnine-completion-map (kbd "C-g") #'tabnine-clear-overlay)
  (define-key tabnine-completion-map (kbd "M-[") #'tabnine-next-completion)
  (define-key tabnine-completion-map (kbd "M-]") #'tabnine-previous-completion)
  )
(provide 'init-company)
