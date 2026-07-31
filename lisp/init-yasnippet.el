(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode))
  :init
  :config
  (progn
    (setq hippie-expand-try-functions-list
	  '(yas/hippie-try-expand
	    try-complete-file-name-partially
	    try-expand-all-abbrevs
	    try-expand-dabbrev
	    try-expand-dabbrev-all-buffers
	    try-expand-dabbrev-from-kill
	    try-complete-lisp-symbol-partially
	    try-complete-lisp-symbol))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Yasnippet Completion At Point Function
(use-package consult-yasnippet
  :ensure t
  :defer t)

;; Auto Yasnippet
(use-package auto-yasnippet
  :ensure t
  :defer t
  :bind (("C-c C-y w"   . aya-create)
	 ("C-c C-y TAB" . aya-expand)
	 ("C-c C-y SPC" . aya-expand-from-history)
	 ("C-c C-y d"   . aya-delete-from-history)
	 ("C-c C-y c"   . aya-clear-history)
	 ("C-c C-y n"   . aya-next-in-history)
	 ("C-c C-y p"   . aya-previous-in-history)
	 ("C-c C-y s"   . aya-persist-snippet)
	 ("C-c C-y o"   . aya-open-line)))

(provide 'init-yasnippet)
