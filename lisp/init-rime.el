(use-package pyim
 :demand t
 :diminish pyim-isearch-mode
 :init
 (setq default-input-method "pyim"
     pyim-title "ㄓ"
     pyim-page-length 5
     ;; 设置 pyim 默认使用的输入法策略，我使用全拼。
     pyim-default-scheme 'quanpin
     pyim-page-tooltip '(posframe popup minibuffer))
 :config
 (setq-default pyim-english-input-switch-functions
		'(pyim-probe-dynamic-english
	    pyim-probe-evil-normal-mode
	    pyim-probe-program-mode
	    pyim-probe-org-structure-template))

 (setq-default pyim-punctuation-half-width-functions
		'(pyim-probe-punctuation-line-beginning
	    pyim-probe-punctuation-after-punctuation))
 (pyim-isearch-mode t)
 :bind
 ("M-f". pyim-forward-word)
 ("M-b". pyim-backward-word)
 ("C-\\". toggle-input-method)
 ;; 金手指设置，可以将光标处的编码（比如：拼音字符串）转换为中文。
 ("M-j" . pyim-convert-string-at-point))

(use-package pyim-greatdict
  :straight (pyim-greatdict :type git
                  :host github
                  :repo "tumashu/pyim-greatdict")
 :config
 (pyim-greatdict-enable))

 (use-package pyim-basedict
 :config
 (pyim-basedict-enable))

;; (use-package rime
;;   :when sys/linuxp
;;   :straight (rime :type git
;;                   :host github
;;                   :repo "DogLooksGood/emacs-rime"
;;                   :files ("*.el" "Makefile" "lib.c"))
;;   :custom
;;   (default-input-method "rime"))

(use-package posframe)

(provide 'init-rime)
