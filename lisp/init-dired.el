;; Directory operations
(with-eval-after-load 'dired
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
	dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  (when sys/macp
    (if (executable-find "gls")
	(progn
	  ;; Use GNU ls as `gls' from `coreutils' if available.
	  (setq insert-directory-program "gls")
	  ;; Using `insert-directory-program'
	  (setq ls-lisp-use-insert-directory-program t))
      (progn
	;; Suppress the warning: `ls does not support --dired'.
	(setq dired-use-ls-dired nil)
	(setq dired-listing-switches "-alh"))))

  ;; Quick sort dired buffers via hydra
  (use-package dired-quick-sort
    :bind (:map dired-mode-map
	    ("S" . hydra-dired-quick-sort/body)))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
	    (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
	    ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  ;; (use-package nerd-icons-dired
  ;;   :diminish
  ;;   :custom-face
  ;;   (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  ;;   :hook (dired-mode . nerd-icons-dired-mode))

  ;; Extra Dired functionality
  ;; (use-package dired-x
  ;;   :ensure nil
  ;;   :demand t
  ;;   :config
  ;;   (let ((cmd (cond (sys/mac-x-p "open")
  ;;		     (sys/linux-x-p "xdg-open")
  ;;		     (sys/win32p "start")
  ;;		     (t ""))))
  ;;     (setq dired-guess-shell-alist-user
  ;;	    `(("\\.pdf\\'" ,cmd)
  ;;	      ("\\.docx\\'" ,cmd)
  ;;	      ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
  ;;	      ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
  ;;	      ("\\.\\(?:xcf\\)\\'" ,cmd)
  ;;	      ("\\.csv\\'" ,cmd)
  ;;	      ("\\.tex\\'" ,cmd)
  ;;	      ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
  ;;	      ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
  ;;	      ("\\.html?\\'" ,cmd)
  ;;	      ("\\.md\\'" ,cmd))))

  ;;   (setq dired-omit-files
  ;;	  (concat dired-omit-files
  ;;		  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))
  )

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))


(provide 'init-dired)
