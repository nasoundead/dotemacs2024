(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
		     ;; fanyi-haici-provider
		     ;; 有道同义词词典
		     fanyi-youdao-thesaurus-provider
		     ;; Etymonline
		     fanyi-etymon-provider
		     ;; Longman
		     ;; fanyi-longman-provider)
		     )))


(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)))

(use-package ace-pinyin
  :straight (ace-pinyin
              :type git
              :host github
              :repo "cute-jumper/ace-pinyin")
  :config
  (ace-pinyin-global-mode +1))

(provide 'init-utils)
