(require 'init-utils)

(defvar sea-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar sea-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar sea-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar sea-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar sea-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

;;
(defvar sea-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `sea/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun sea/escape ()
  "Run the `sea-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall sea-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'sea/escape)

;;
;; General
(use-package general)
;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'unmap! #'general-unbind)

;; leader/localleader keys
(define-prefix-command 'sea-leader 'sea-leader-map)
(defvar sea-leader-alist `((t . ,sea-leader-map)))
(add-to-list 'emulation-mode-map-alists 'sea-leader-alist)

;; We avoid `general-create-definer' to ensure that :states, :prefix and
;; :keymaps cannot be overwritten.
(defmacro define-leader-key! (&rest args)
  `(general-define-key
    :states nil
    :keymaps 'sea-leader-map
    :prefix sea-leader-alt-key
    ,@args))

(general-create-definer define-localleader-key!
  :major-modes t
  :wk-full-keys nil
  :prefix sea-localleader-alt-key)

;; Because :non-normal-prefix doesn't work for non-evil sessions (only evil's
;; emacs state), we must redefine `define-localleader-key!' once evil is loaded
(after! evil
  (defmacro define-leader-key! (&rest args)
    `(general-define-key
      :states '(normal visual motion insert)
      :keymaps 'sea-leader-map
      :prefix sea-leader-key
      :non-normal-prefix sea-leader-alt-key
      ,@args))

  (general-create-definer define-localleader-key!
    :states (cdr general-describe-evil-states)
    :major-modes t
    :wk-full-keys nil
    :prefix sea-localleader-key
    :non-normal-prefix sea-localleader-alt-key))

;;
;; Packages


;; `hydra'
(setq lv-use-seperator t)

(defvar sea-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun sea--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`sea-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l sea-evil-state-alist)) collect it
           else do (error "not a valid state: %s" l)))

;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :keymap       'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :unless       'lisp-indent-function 'defun)
(put :if           'lisp-indent-function 'defun)
(put :when         'lisp-indent-function 'defun)

;; specials
(defvar sea--map-forms nil)
(defvar sea--map-fn nil)
(defvar sea--map-batch-forms nil)
(defvar sea--map-state '(:dummy t))
(defvar sea--map-parent-state nil)
(defvar sea--map-evil-p nil)
(after! evil (setq sea--map-evil-p t))

(defun sea--map-process (rest)
  (let ((sea--map-fn sea--map-fn)
        sea--map-state
        sea--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (sea--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (sea--map-commit)
                  (setq sea--map-fn 'define-leader-key!))
                 (:localleader
                  (sea--map-commit)
                  (setq sea--map-fn 'define-localleader-key!))
                 (:after
                  (sea--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 ((or :map :map* :keymap)
                  (sea--map-set :keymaps `(quote ,(sea-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (sea-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :if :when :unless)
                  (sea--map-nested (list (intern (sea-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc) (sea-enlist (pop rest))
                    (sea--map-set (if sea--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          sea--map-forms)))
                 (_
                  (condition-case e
                      (sea--map-def (pop rest) (pop rest) (sea--keyword-to-states key) desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((sea--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (sea--map-commit)
    (macroexp-progn (nreverse (delq nil sea--map-forms)))))

(defun sea--map-append-keys (prop)
  (let ((a (plist-get sea--map-parent-state prop))
        (b (plist-get sea--map-state prop)))
    (if (and a b)
        `(general--concat nil ,a ,b)
      (or a b))))

(defun sea--map-nested (wrapper rest)
  (sea--map-commit)
  (let ((sea--map-parent-state (sea--map-state)))
    (push (if wrapper
              (append wrapper (list (sea--map-process rest)))
            (sea--map-process rest))
          sea--map-forms)))

(defun sea--map-set (prop &optional value)
  (unless (equal (plist-get sea--map-state prop) value)
    (sea--map-commit))
  (setq sea--map-state (plist-put sea--map-state prop value)))

(defun sea--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (sea-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(nil :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state sea--map-batch-forms)))
  t)

(defun sea--map-commit ()
  (when sea--map-batch-forms
    (cl-loop with attrs = (sea--map-state)
             for (state . defs) in sea--map-batch-forms
             if (or sea--map-evil-p (not state))
             collect `(,(or sea--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) sea--map-forms))
    (setq sea--map-batch-forms nil)))

(defun sea--map-state ()
  (let ((plist
         (append (list :prefix (sea--map-append-keys :prefix)
                       :infix  (sea--map-append-keys :infix)
                       :keymaps
                       (append (plist-get sea--map-parent-state :keymaps)
                               (plist-get sea--map-state :keymaps)))
                 sea--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

Properties
  :leader [...]                   an alias for (:prefix sea-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :keymap [KEYMAP(s)] [...]       same as :map
  :prefix [PREFIX] [...]          set keybind prefix for following keys
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :if [CONDITION] [...]
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

Example
  (map! :map magit-mode-map
        :m  \"C-r\" 'do-something           ; C-r in motion state
        :nv \"q\" 'magit-mode-quit-window   ; q in normal+visual states
        \"C-x C-r\" 'a-global-keybind
        :g \"C-x C-r\" 'another-global-keybind  ; same as above

        (:when IS-MAC
         :n \"M-s\" 'some-fn
         :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (sea--map-process rest))


(provide 'init-keybindings)