(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(use-package py-autopep8
  :init
  (add-hook 'python-mode-hook 'py-autopep8-mode))


(provide 'init-py)
