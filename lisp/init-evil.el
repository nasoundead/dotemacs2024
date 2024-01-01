(use-package evil
  :straight (:host github
             :repo "emacs-evil/evil")
  :init
  (setq evil-magic 'very-magic)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))


(use-package evil-escape
  :straight (:host github
             :repo "syl20bnr/evil-escape")
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  :config
  (evil-escape-mode))

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(provide 'init-evil)
