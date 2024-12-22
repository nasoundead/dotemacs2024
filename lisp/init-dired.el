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


(with-eval-after-load 'dired
  (defun my-dired-init ()
	"to be run as hook for `dired-mode'."
	;; (dired-hide-details-mode 1)
	;; Guess a default target directory
	(setq dired-dwim-target t)

	;; Always delete and copy recursively
	(setq dired-recursive-deletes 'always
		dired-recursive-copies 'always)

	;; Show directory first
	(setq dired-listing-switches "-alh --group-directories-first")

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

	;; `find-dired' alternative using `fd'
	(when (executable-find "fd")
		(use-package fd-dired))
	)

	(add-hook 'dired-mode-hook 'my-dired-init)

	(when (>= emacs-major-version 28)
		(setq dired-kill-when-opening-new-dired-buffer t))
	
	(when (< emacs-major-version 28)
		(progn
			(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
			(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))
)


(provide 'init-dired)
