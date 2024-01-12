;;;; lisp/core


(defun sea--icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and (or (display-graphic-p) (daemonp))
       (or (featurep 'all-the-icons)
           (require 'all-the-icons nil t))))

(defun sea-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun sea-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun sea--resolve-hook-forms (hooks)
  (declare (pure t) (side-effect-free t))
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (sea-enlist (sea-unquote hooks))
           if (eq (car-safe hook) 'quote)
            collect (cadr hook)
           else if quoted-p
            collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (sea--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(if append-p (nreverse forms) forms)))))


(defmacro after! (targets &rest body)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation. This will no-op on features that have been disabled by the user."
  (declare (indent defun) (debug t))
  (unless (and (symbolp targets)
               (memq targets (bound-and-true-p sea-disabled-packages)))
    (list (if (or (not (bound-and-true-p byte-compile-current-file))
                  (dolist (next (sea-enlist targets))
                    (unless (keywordp next)
                      (if (symbolp next)
                          (require next nil :no-error)
                        (load next :no-message :no-error)))))
              #'progn
            #'with-no-warnings)
          (if (symbolp targets)
              `(with-eval-after-load ',targets ,@body)
            (pcase (car-safe targets)
              ((or :or :any)
               (macroexp-progn
                (cl-loop for next in (cdr targets)
                         collect `(after! ,next ,@body))))
              ((or :and :all)
               (dolist (next (cdr targets))
                 (setq body `((after! ,next ,@body))))
               (car body))
              (_ `(after! (:and ,@targets) ,@body)))))))

(defun sea-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun sea-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type :test keyword)
  (substring (symbol-name keyword) 1))

(defun sea-log (format-string &rest args)
  "Log to *Messages* if `sea-debug-mode' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same arguments as `message'."
  `(when sea-debug-mode
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "sea " 'face 'font-lock-comment-face)
                 (when (bound-and-true-p sea--current-module)
                   (propertize
                    (format "[%s/%s] "
                            (sea-keyword-name (car sea--current-module))
                            (cdr sea--current-module))
                    'face 'warning))
                 format-string)
        ,@args))))

(defun FILE! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        (buffer-file-name)
        ((stringp (car-safe current-load-list)) (car current-load-list))))

(defun DIR! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (let ((file (FILE!)))
    (and file (file-name-directory file))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (unless path
    (setq path (or (DIR!)
                   (error "Could not detect path to look for '%s' in"
                          filename))))
  (let ((file (if path `(expand-file-name ,filename ,path) filename)))
    `(condition-case e
         (load ,file ,noerror ,(not sea-debug-mode))
       ((debug sea-error) (signal (car e) (cdr e)))
       ((debug error)
        (let* ((source (file-name-sans-extension ,file))
               (err (cond 
                          ((cons 'sea-module-error sea-emacs-dir)))))
          (signal (car err)
                  (list (file-relative-name
                         (concat source ".el")
                         (cdr err))
                        e)))))))


(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (sea-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       ,(when where-alist
          `(dolist (targets (list ,@(nreverse where-alist)))
             (dolist (target (cdr targets))
               (advice-add target (car targets) #',symbol)))))))


;; 自动扫描 .emacs.d/lisp/core 下的 .el 文件，然后把它编译到 .emacs.d/lisp/core/.build/ 下面。
;; 收集这些文件里加了 ;;;###autoload 的定义，创建.emacs.d/lisp/core/.build/autoloads.el 并且自动加载。
;; 把 .emacs.d/site-lisp/.build/ 加到 load-path，这样就可以用 require 或 use-package 了。
;; 按需更新，在 site-lisp 下有任何 .el 文件有改动的情况下才会重做整个流程，平时几乎不耗时。

;; The lisp/core directory is where we put our own packages.  We byte-compile
;; and generate an autoload file for them. We only do this when a package is
;; newer than its byte-compiled version.

;; This is needed, or `generated-autoload-file' will be not defined as a
;; variable at byte-compile time.  See the comments in
;; `straight--generate-package-autoloads'.
(eval-and-compile
  (require 'autoload)
  (require 'bytecomp))

(let* (;; Dir & files
       (core-lisp-dir (concat user-emacs-directory "lisp/core/"))
       (build-dir (progn (make-directory (concat core-lisp-dir ".build/") t)
                         (concat core-lisp-dir ".build/")))
       (build-files (directory-files build-dir))
       (newer-lisp-file nil)
       ;; Don't bother me.
       (inhibit-message t)
       ;; Prevent `update-directory-autoloads' from running hooks when visiting
       ;; the autoload file.
       (find-file-hook nil)
       (write-file-functions nil)
       ;; Prevent `update-directory-autoloads' from creating backup files.
       (backup-inhibited t)
       (version-control 'never)
       (generated-autoload-file (concat build-dir "autoloads.el")))
  (cl-letf (((symbol-function #'byte-compile-log-1) #'ignore)
            ((symbol-function #'byte-compile-log-file) #'ignore)
            ((symbol-function #'byte-compile-log-warning) #'ignore))
    (add-to-list 'load-path build-dir)
    (dolist (file (directory-files core-lisp-dir))
      (unless (string-prefix-p "." file)
        ;; Make symlinks of site-lisp files in build-dir.  This is needed for
        ;; `byte-compile-file' and `update-directory-autoloads'.
        (unless (member file build-files)
          (make-symbolic-link (expand-file-name (concat core-lisp-dir file))
                              (expand-file-name (concat build-dir file))))
        ;; Byte compile
        (let ((byte-file (concat build-dir
                                 (file-name-sans-extension file)
                                 ".elc")))
          (when (file-newer-than-file-p (concat core-lisp-dir file) byte-file)
            (setq newer-lisp-file t)
            (byte-compile-file (concat build-dir file))))))
    ;; Generate autoload file
    (when newer-lisp-file
      (unless (file-exists-p generated-autoload-file)
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (insert ";; -*- lexical-binding: t -*-\n")
          (save-buffer)))
      (update-directory-autoloads build-dir)
      (byte-compile-file (concat build-dir "autoloads.el")))
    ;; Load autoload file
    (load (concat build-dir "autoloads") 'noerror 'nomessage)))


(provide 'init-core)