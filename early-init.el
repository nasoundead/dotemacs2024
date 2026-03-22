;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; 启动优化 (这一行必须在最顶上)
(setq gc-cons-threshold most-positive-fixnum)

;; UI 提前禁用 (防止闪烁)
(setq inhibit-startup-screen t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 针对 Linux/EndeavourOS 的渲染优化
(setq frame-inhibit-implied-resize t) ; 禁止不必要的窗口重绘
(setq inhibit-compacting-font-caches t) ; 禁止回收字体缓存，用空间换时间

;; 砍掉启动欢迎语，直接进正题
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;; 包管理器提前禁能 (让 init.el 里的 use-package 手动接管，省时间)
(setq package-enable-at-startup nil)

;; 性能基础
(setq warning-minimum-level :error) ; 减少启动时的弹窗干扰

;; 强制使用不带闪烁的渲染模式
(advice-add #'display-startup-screen :override #'ignore)

;; 最后一个小锦囊 (提升 0.05s) 如果你发现那 48 个包安装完后，启动时间略有回升，可以尝试在 early-init.el 里的最后一行加上这个（如果还没加 的话）
;; 暂时禁用文件加载时的垃圾回收
(setq gc-cons-threshold most-positive-fixnum)

;; 并在 init.el 的末尾把它调回来
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold (* 8 1024 1024))))

;; 让渲染更连贯
(setq-default redisplay-dont-pause t)

;; 启动时把 GC 关了，启动后再开。
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 8 1024 1024)))) ;; 恢复到 8MB

;; 禁用文件处理器查询
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (setq file-name-handler-alist old-file-name-handler-alist))))



(defconst sys/winp
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defvar sea-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all sea functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

;;; Directories/files
(defvar sea-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst sea-core-dir
  (expand-file-name "lisp/" user-emacs-directory)
  "core directory.")

(defconst sea-bin-dir
  (expand-file-name "bin/" user-emacs-directory)
  "bin directory.")

(defconst sea-etc-dir
  (expand-file-name "etc/" user-emacs-directory)
  "etc directory.")

(defconst sea-cache-dir
  (expand-file-name ".cache/" user-emacs-directory)
  "Cache directory.")

(defconst sea-site-lisp-dir
  (expand-file-name "site-lisp/" user-emacs-directory)
  "site-lisp directory.")

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path sea-core-dir)
;; (add-subdirs-to-load-path sea-site-lisp-dir)
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/blink-search/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/simple-modeline/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/unicode-fonts/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

;; (setq warning-minimum-level :error)
(setq native-comp-jit-compilation nil)
(setq byte-compile-warnings '(not nresolved
			      free-vars
			      callargs
			      redefine
			      obsolete
			      noruntime
			      cl-functions
			      interactive-only
			      ))
