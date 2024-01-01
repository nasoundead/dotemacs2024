(require 'init-package)
(require 'init-better-defaults)
(require 'init-project)
(require 'init-evil)
(require 'init-vertico)
(require 'init-corfu)
(require 'init-utils)
(require 'init-hydra)
(require 'init-folding)

;; (require 'init-key)

(require 'init-flycheck)
(require 'init-ui)
(require 'init-modeline)
(when (display-graphic-p)
  (require 'init-font))

;; (require 'init-flycheck)
;; (require 'init-lsp)
;; (require 'init-treesit)
;; (require 'init-minibuffer)
;; (require 'init-corfu)

;; (require 'init-org)
;; (require 'init-web)


(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t t)
