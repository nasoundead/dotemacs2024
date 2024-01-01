(use-package evil
  :custom
  (evil-magic 'very-magic)
  (evil-search-module 'evil-search)
  (evil-ex-search-vim-style-regexp t)
  (evil-want-keybinding nil)
  :init
  (evil-mode 1)
  )


(use-package evil-escape
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  :init
  (evil-escape-mode))

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :init (evil-commentary-mode 1))

(provide 'init-evil)
