(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)))

 (use-package ace-pinyin
  :straight (ace-pinyin :type git :host github :repo "nasoundead/ace-pinyin")
  :defer t
  :config
  (ace-pinyin-global-mode +1))

;; (use-package ace-pinyin
;;   :straight (ace-pinyin
;;               :type git
;;               :host github
;;               :repo "cute-jumper/ace-pinyin")
;;   :config
;;   (ace-pinyin-global-mode +1))

(provide 'init-navi)