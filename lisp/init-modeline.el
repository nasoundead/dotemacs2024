;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;
;; (use-package nyan-mode)
;; (add-hook 'sea-init-ui-hook #'nyan-mode)

(require 'simple-modeline)
(simple-modeline-mode)

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; (use-package mood-line
;;   :hook  (after-init-hook . mood-line-mode)
;;   :custom
;;   (mood-line-glyph-alist  mood-line-glyphs-fira-code)
;;   (mode-line-misc-info '((which-function-mode
;;                           (which-func-mode
;;                            ("" which-func-format " "))))))

(provide 'init-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
