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

;; (require 'init-lsp)
(require 'init-treesitter)

;; (require 'init-org)
;; (require 'init-web)


(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t t)
(dolist (dir (list sea-cache-dir sea-etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))