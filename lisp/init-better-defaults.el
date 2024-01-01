(setq initial-buffer-choice t)
(setq user-full-name "nasoundead")       ;设置用户名
(setq user-mail-address
      "nasoundead@163.com") 	;设置邮箱
(setq use-dialog-box nil)               ;never pop dialog
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)      ;阻止退出emacs时和外部clipmgr交互导致卡顿 x11
(setq select-enable-clipboard t)        ; wayland 复制粘贴共享
(setq split-width-threshold 180)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq word-wrap-by-category t)          ;按照中文折行
(setq garbage-collection-messages t)	;gc时显示消息
(setq byte-compile-warnings nil)	    ;关闭字节编译警告
(setq load-prefer-newer nil)
(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
;; 增加长行处理性能
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq yes-or-no-prompt "(y or n)")

;; 关闭自动备份文件
(setq make-backup-files nil)
;; 关闭启动页
(setq inhibit-startup-screen nil)
;; 关闭dir local
(setq enable-dir-local-variables t)
(setq scroll-margin 0); 设定滚动边距
(setq-default 
  truncate-lines t     ; 不要换行
  vc-follow-symlinks t
  ;; Save clipboard contents into kill-ring before replacing them
  save-interprogram-paste-before-kill t
  ) 
(setq truncate-partial-width-windows nil)
(setq fill-column 180)
(setq lexical-binding t)

;; 环境变量
(setq-default recentf-max-saved-items 1000)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)
;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)


(delete-selection-mode 1)
(electric-pair-mode)


;; savehist / saveplace
(setq savehist-file (concat sea-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat sea-cache-dir "saveplace"))
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

;; Keep track of recently opened files
(use-package recentf
  :init
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  :config
  (setq recentf-save-file (concat sea-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private sea temp files (but not all of them)
              (concat "^" (file-truename sea-cache-dir)))))

(when sys/winp
  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)

  (setq w32-apps-modifier 'hyper)
  (setq w32-rwindow-modifier 'hyper)
  )

(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))
;; coding
(defun windows-shell-mode-coding ()
  (set-buffer-file-coding-system 'gbk)
  (set-buffer-process-coding-system 'gbk 'gbk))
(defun python-encode-in-org-babel-execute (func body params)
    (let ((coding-system-for-write 'utf-8))
      (funcall func body params)))
(cond
 ((eq system-type 'sys/winp)
  (set-language-environment "chinese-gbk")
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'gbk)
  (modify-coding-system-alist 'process "*" 'gbk)
  (add-hook 'shell-mode-hook #'windows-shell-mode-coding)
  (add-hook 'inferior-python-mode-hook #'windows-shell-mode-coding)
  (advice-add #'org-babel-execute:python :around
              #'python-encode-in-org-babel-execute))
 (t
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)))


;; Environment
(when (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))


(defcustom sea/buffer-skip-regexp
  (rx bos
      (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
              "*Messages*" "*scratch*" "*Help*"
              "*package*" "*Warnings*"
              "*Async-native-compile-log*")
          (seq "magit-diff" (zero-or-more anything))
          (seq "magit-process" (zero-or-more anything))
          (seq "magit-revision" (zero-or-more anything))
          (seq "magit-stash" (zero-or-more anything)))
      eos)
  "Regular expression matching buffers ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp)
(defun sea/buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `sea/buffer-skip-regexp'."
  (string-match-p sea/buffer-skip-regexp (buffer-name buffer)))
(setq switch-to-prev-buffer-skip 'sea/buffer-skip-p)


(use-package display-line-numbers
  :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))


(use-package ido-vertical-mode
  :init
  (ido-vertical-mode 1)
  :config
  (setq ido-vertical-show-count 1))


(setq kill-ring-max 200)
;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

(global-hl-line-mode)

;; Kill & Mark things easily
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Interactively insert items from kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :init (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings))

(use-package dash
  :ensure t
  :defer t)
(use-package f
  :ensure t
  :defer t)
(use-package s
  :ensure t
  :defer t)
(use-package eldoc-eval)
(use-package shrink-path
  :commands (shrink-path-prompt shrink-path-file-mixed))

(use-package anzu
  :init (global-anzu-mode +1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> ")))

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "%s/%s ")
(setq lazy-highlight-cleanup nil)
;; 这样可以在literal的isearch中，把空格直接当成正则里面的.*匹
(setq isearch-lax-whitespace t)
(setq search-whitespace-regexp ".*")
;; 默认的isearch-forward函数是literal的，也就是用户输入什么就匹配什么，没有正则解释，没有转义，完全literal。这样的好处就是，想搜啥就是啥，不用考虑太多。其实默认用它就可以了。
;; 当然，完全可以开启正则匹配等功能，下面就说说这几个toggle函数。
;; isearch-toggle-regexp 在使用isearch搜索时（即按下C-s isearch-forward后）绑定到M-s r。 按下后，您的输入全部都会被以正则来匹配了。
;; isearch-toggle-case-fold 默认绑到M-s c。默认isearch对大小写是类似于rg一样“smart”的。具体地说，如果用户全部输入小写，则不匹分大小写进行匹配，如果用户输入中包括大写，则精确匹配大小写。 再举个例子，默认情况下，默认foo可以匹配foo,Foo,FOO。输入Foo，只能匹配到Foo。打开这个选项后，就是case sensitive了，也就只能精确匹配了。个人认为，该选项用处不太大。
;; isearch-toggle-word 默认绑定到M-s w。打开word匹配。直接举例：未打开以前，foo可以匹配foobar，foo。打开该选项后，foo只能匹配foo了，foobar就匹配不到了。 可以看出来，开启该选项后，isearch必须完全匹配一个完整地word。这个功能可以帮忙过滤很多杂项。
;; isearch-toggle-symbol 默认绑定到M-s _ 。它和isearch-toggle-word的基本一样，不过它使isearch完全匹配一个symbol。 简单来说，symbol和word的区别：isearch-toggle-word是一个symbol，它包括isearch toggle和word三个word。
;; isearch-toggle-lax-whitespace 默认绑定到M-s SPC。开启该功能后，可以把输入中的空格当做一个固定的正则表达式，这个固定的正则表达式存在于search-whitespace-regexp变量中。关于这个功能，我在后面 空格的特殊用法 中进行详细说明。
(with-eval-after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
  ;; Activate occur easily inside isearch
  (when (fboundp 'isearch-occur)
    ;; to match ivy conventions
    (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur))

;;   (define-key isearch-mode-map (kbd "<C-return>") 'swiper-from-isearch)

  (defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search))))


;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package aggressive-indent
  :init
  (dolist (hook '(emacs-lisp-mode-hook css-mode-hook))
    (add-hook hook #'aggressive-indent-mode)))

;; Increase selected region by semantic units
(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package pcre2el
  :commands rxt-quote-pcre
  :init (add-hook 'after-init-hook #'rxt-global-mode))
(use-package ialign
  :init
  (setq ialign-pcre-mode t)
  (setq ialign-initial-group -1)
  (setq ialign-initial-repeat t)
  (setq ialign-initial-regexp "([ ,=])"))
;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq
    undo-tree-auto-save-history nil
    undo-tree-history-directory-alist `(("." . ,(concat sea-cache-dir "undo/")))))

(use-package hungry-delete)
(global-hungry-delete-mode)

(provide 'init-better-defaults)
;;; base ends here