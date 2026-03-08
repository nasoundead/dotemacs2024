
;; ==========================================
;; [1] elpaca
;; ==========================================
(custom-set-faces
 '(elpaca-finished          ((t (:family "KN Bobohei" :foreground "#eceff1" :weight bold             ))))  ; [finished]: 雪羽白，清爽洁净感
 '(elpaca-busy              ((t (:family "KN Bobohei" :foreground "#a9d0e5" :slant italic            ))))  ; [busy]: 苏打蓝，带点呼吸感的闪烁感
 '(elpaca-blocked           ((t (:family "KN Bobohei" :foreground "#455a64"                          )))) ; [block]: 等待别的包先完成的状态，暗蓝调
 '(elpaca-failed            ((t (:family "KN Bobohei" :foreground "#b39ddb" :weight bold :underline t ))))  ; [failed]: 迷雾紫红调
 '(elpaca-log-error         ((t (:family "KN Bobohei" :foreground "#b39ddb" :background "#1a232e"      ))))  ; error日志: 醒目的迷雾紫罗兰
 '(elpaca-log-info          ((t (:family "KN Bobohei" :foreground "#2c3e50"                          ))))  ; info信息: 波子汽水蓝，流畅感
 '(elpaca-log-highlight     ((t (:family "KN Bobohei" :foreground "#eceff1" :weight bold              ))))  ; highlight信息: 雪羽白
 ;; 手动干预的 Elpaca UI 操作标记 (Marked Actions)
 '(elpaca-ui-marked-install ((t (:family "KN Bobohei" :foreground "#a9d0e5" :inherit bold            )))) ; 亮蓝安装
 '(elpaca-ui-marked-delete  ((t (:family "KN Bobohei" :foreground "#455a64" :strike-through t        )))) ; 暗灰删除
 '(elpaca-ui-marked-fetch   ((t (:family "KN Bobohei" :foreground "#b39ddb"                          ))))
 '(elpaca-ui-marked-pull    ((t (:family "KN Bobohei" :foreground "#b39ddb" :weight bold             ))))
 '(elpaca-ui-marked-merge   ((t (:family "KN Bobohei" :foreground "#eceff1" :background "#2c3e50"    ))))
 '(elpaca-ui-marked-rebuild ((t (:family "KN Bobohei" :foreground "#b0bec5" :slant italic            ))))
 ;; 冲突: 全表最警示的颜色，用深巷蓝背景衬托雪羽白
 '(elpaca-ui-conflicting    ((t (:family "KN Bobohei" :foreground "#eceff1" :background "#455a64" :weight bold)))))


(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))  ; 绝对路径
       (build (expand-file-name "elpaca/" elpaca-builds-directory)) ; 绝对路径
       (order (cdr elpaca-order))
       (autoloads-file (expand-file-name "elpaca-autoloads.el" repo))) ; 补全后缀+绝对路径
  ;; 1. 修复 load-path：优先用 build 目录，其次 repo 目录
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  
  ;; 2. 克隆/编译 Elpaca（适配 Windows Git 调用）
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x nil t)) ; 兼容 Emacs 28
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ;; 修复 Windows Git 调用：用 executable-find 找 git 可执行文件
                  (git (executable-find "git"))
                  ((zerop (apply #'call-process `(,git nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process git nil buffer t "checkout"
                                        (or (plist-get order :ref) "HEAD")))) ; 替换 -- 为 HEAD（Windows Git 兼容）
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" repo "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca nil t))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "Elpaca 引导成功：%s" (buffer-string)) (kill-buffer buffer))
          (error "Elpaca 引导失败：%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "Elpaca 引导异常：%s" err) (delete-directory repo 'recursive))))
  
  ;; 3. 修复 elpaca-autoloads 加载（核心：绝对路径+补全后缀+移除无效变量）
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca nil t)
    (elpaca-generate-autoloads "elpaca" repo)
    ;; 修复：用绝对路径加载，补全 .el 后缀，移除 load-source-file-function（Windows 兼容）
    (load autoloads-file nil t)))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; 并发cloning
;; (setq elpaca-queue-limit 3) ;; 默认是 5 或更大，改成 2 或者 1 试试最稳

;; 禁用 package.el
(setq package-enable-at-startup nil)

;; 安装 use-package 支持 (这样你以前的 use-package 配置基本不用改)
(elpaca elpaca-use-package
  ;; 启用 use-package 集成
  (elpaca-use-package-mode)
  ;; 设置 :ensure 默认使用 elpaca (是 :ensure (:host xx) 而不是 :elpaca (:host xx))
  (setq use-package-always-ensure t))

(provide 'init-elpaca)