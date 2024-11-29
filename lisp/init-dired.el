(use-package ztree
  :defer t
  :ensure t)

(defun sea/dired-diff ()
  "Ediff marked files in dired or selected files in separate window"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (if (and (file-directory-p (nth 0 marked-files))
                    (file-directory-p (nth 1 marked-files)))
               (ztree-diff (nth 0 marked-files)
                           (nth 1 marked-files))
             (ediff-files (nth 0 marked-files)
                          (nth 1 marked-files))))
          ((= (length marked-files) 3)
           (ediff-files3 (nth 0 marked-files)
                         (nth 1 marked-files)
                         (nth 2 marked-files)
                         ))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (if (and (file-directory-p (nth 0 marked-files))
                    (file-directory-p (nth 0 other-marked-files)))
               (ztree-diff (nth 0 marked-files)
                           (nth 0 other-marked-files)))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (dired-diff))
          (t (error "mark exactly 2 files, at least 1 locally")))))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))
(eval-after-load 'dired '(lambda () (;; Always delete and copy recursively
                                     (setq dired-recursive-deletes 'always
                                           dired-recursive-copies 'always)
                                     ;; we want dired not not make always a new buffer if visiting a directory
                                     ;; but using only one dired buffer for all directories.
                                     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
                                       "Replace current buffer if file is a directory."
                                       (interactive)
                                       (let ((orig (current-buffer))
                                             (filename (dired-get-filename)))
                                         ad-do-it
                                         (when (and (file-directory-p filename)
                                                    (not (eq (current-buffer) orig)))
                                           (kill-buffer orig))))
                                     (when  IS-MAC
                                       ;; Suppress the warning: `ls does not support --dired'.
                                       (setq dired-use-ls-dired nil)

                                       (when (executable-find "gls")
                                         ;; Use GNU ls as `gls' from `coreutils' if available.
                                         (setq insert-directory-program "gls")))

                                     (when (or (and IS-MAC (executable-find "gls"))
                                               (and (or IS-LINUX IS-MAC) (executable-find "ls")))
                                       ;; Using `insert-directory-program'
                                       (setq ls-lisp-use-insert-directory-program t)
                                       ;; Show directory first
                                       (setq dired-listing-switches "-alh --group-directories-first"))
                                     )))


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
