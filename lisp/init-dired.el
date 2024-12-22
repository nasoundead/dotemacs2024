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
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "C-x C-d") 'dired-diff)
;;   (define-key dired-mode-map (kbd "C-x C-e") 'dired-ediff-files)
;;   (define-key dired-mode-map (kbd "C-x C-r") 'dired-do-rsync))


;; Directory operations
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'dired-noselect-mode)
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

)

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))


(provide 'init-dired)
