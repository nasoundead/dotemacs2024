;;; init-ui.el -*- lexical-binding: t; -*-
(defvar sea-init-ui-hook nil
  "ui hook")

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
(setq icon-title-format frame-title-format)

(when emacs/>=29p
  (pixel-scroll-precision-mode t)
)

(setq custom-safe-themes t)
(use-package color-theme-sanityinc-tomorrow)
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-day))
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-day))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)
(add-hook 'after-make-frame-functions 'reapply-themes)
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(use-package modus-themes
  :straight (modus-themes
	     :type git
	     :host github
	     :repo "protesilaos/modus-themes")
  ;; :config
  ;; (load-theme 'modus-vivendi-tinted :no-confirm-loading)
  )

(use-package mindre-theme
  :ensure t
  :straight (:host github :repo "erikbackman/mindre-theme")
  :custom
  (mindre-use-more-bold nil)
  (mindre-use-faded-lisp-parens t)
  :config
  (load-theme 'mindre t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (setq doom-themes-treemacs-theme "doom-colos") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'sea-init-ui-hook #'show-paren-mode)

;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'sea-init-ui-hook #'winner-mode)


;; postframe
(use-package posframe)


;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  (setq winner-boring-buffers '("*Completions*"
				"*Compile-Log*"
				"*inferior-lisp*"
				"*Fuzzy Completions*"
				"*Apropos*"
				"*Help*"
				"*cvs*"
				"*Buffer List*"
				"*Ibuffer*"
				"*esh command on file*")))



(reapply-themes)
(run-hooks 'sea-init-ui-hook)

(use-package switch-window
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'switch-window))

(use-package windmove
  :ensure nil
  :init (add-hook 'sea-init-ui-hook #'windmove-default-keybindings))

(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'column)
  ;; (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  :hook ((org-indent-mode-hook)
	    . (lambda ()
		(when highlight-indent-guides-mode
			(highlight-indent-guides-mode -1)))))

(use-package page-break-lines
  :hook ((prog-mode text-mode conf-mode) . page-break-lines-mode)
  )

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Sea Emacs. Enjoy!")

  ;; Set the banner
  ;; (setq dashboard-startup-banner 'logo)
  (setq dashboard-startup-banner (or sea-logo 'official))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)

  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 9)
			  (bookmarks . 5)
			  (projects . 9)
			  (agenda . 5)
			  (registers . 5)))
  (setq show-week-agenda-p t)
  )

;; (set-frame-parameter nil 'alpha 0.95)

;; (use-package nerd-icons-buffer-menu
;;   :straight (nerd-icons-buffer-menu :type git :host github :repo "jcs-elpa/nerd-icons-buffer-menu")
;;   :hook (Buffer-menu-mode . nerd-icons-buffer-menu-mode))

;; Minibuffer completion icons
;; (require 'nerd-svg-icons-completion)
;; (nerd-svg-icons-completion-mode t)
;; ;; Dired icons
;; (require 'nerd-svg-icons-dired)
;; (add-hook 'dired-mode-hook 'nerd-svg-icons-dired-mode)
;; ;; Ibuffer icons
;; (add-hook 'ibuffer-hook #'nerd-svg-icons-ibuffer-mode)

;; (use-package nerd-icons
;;  :straight (nerd-icons
;;	   :type git
;;	   :host github
;;	   :repo "rainstormstudio/nerd-icons.el"
;;	   :files (:defaults "data" "nerd-icons-pkg.el"))
;;  :custom
;;  ;; The Nerd Font you want to use in GUI
;;  ;; "Symbols Nerd Font Mono" is the default and is recommended
;;  ;; but you can use any other Nerd Font if you want
;;  (nerd-icons-font-family "Symbols Nerd Font Mono")
;;  )

(provide 'init-ui)
