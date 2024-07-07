;; (require 'init-package)
(require 'init-straight)

(require 'init-core)
(require 'init-utils)

(require 'init-better-defaults)
(require 'init-project)
(require 'init-dired)

(require 'init-evil)
(require 'init-keybindings)
(require 'init-vertico)
(require 'init-corfu)
(require 'init-lookup)
(require 'init-hydra)
(require 'init-folding)
(require 'init-flycheck)
(require 'init-vcs)
(require 'init-ui)
(require 'init-modeline)
(require 'init-highlight)
(when (display-graphic-p)
  (require 'init-font))
(require 'init-treemacs)
(require 'init-treesitter)
(require 'init-lsp)
(require 'init-org)
(require 'init-py)
(require 'init-go)
(require 'init-rust)


(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t t)
(dolist (dir (list sea-cache-dir sea-etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))
