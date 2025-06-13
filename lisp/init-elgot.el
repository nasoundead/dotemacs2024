(use-package eglot
  :defer t
  :commands (eglot-ensure my/rust-expand-macro)
  :hook (eglot-managed-mode . my/eglot-hook)
  :config
  (progn
    (defun my/eglot-hook ()
      ;; Show flymake diagnostics first.
      ;; https://github.com/joaotavora/eglot/discussions/898#discussioncomment-2609402
      (setq eldoc-documentation-functions
            (cons #'flymake-eldoc-function
                  (remove #'flymake-eldoc-function eldoc-documentation-functions))))

    (setq eldoc-echo-area-use-multiline-p 3
          eldoc-echo-area-display-truncation-message nil)
    (set-face-attribute 'eglot-highlight-symbol-face nil
                        :background "#b3d7ff")

    ;; https://github.com/joaotavora/eglot/pull/901
    ;; 2022-05-28 后支持
    (add-to-list 'eglot-server-programs `(rust-mode . ("rust-analyzer"
                                                       :initializationOptions (:cargo (:features "all")))))
    (defun my/rust-expand-macro ()
      "Expand macro at point, same as `lsp-rust-analyzer-expand-macro'.
https://rust-analyzer.github.io/manual.html#expand-macro-recursively"
      (interactive)
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :rust-analyzer/expandMacro (eglot--TextDocumentPositionParams)
       :error-fn (lambda (msg) (error "Macro expand failed, msg:%s." msg))
       :success-fn
       (lambda (expanded-macro)
	     (cl-destructuring-bind (name format expansion result) expanded-macro
	       (let* ((pr (eglot--current-project))
			      (buf (get-buffer-create (format "*rust macro expansion %s*" (project-root pr)))))
		     (with-current-buffer buf
		       (let ((inhibit-read-only t))
			     (erase-buffer)
			     (insert result)
			     (rust-mode)))
		     (switch-to-buffer-other-window buf))))))
    ))

(provide 'init-elgot)