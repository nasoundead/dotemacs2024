;; (use-package pyim
;;   :demand t
;;   :diminish pyim-isearch-mode
;;   :init
;;   (setq default-input-method "pyim"
;;         pyim-title "ㄓ"
;;         pyim-default-scheme 'rime
;;         pyim-page-length 7
;;         pyim-page-tooltip 'proframe)
;;   :config
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-evil-normal-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))
;;   (pyim-isearch-mode t)
;;   :bind ("M-j" . pyim-convert-string-at-point))

(use-package rime
  :custom
  (default-input-method "rime"))

(use-package posframe)

(provide 'init-rime)