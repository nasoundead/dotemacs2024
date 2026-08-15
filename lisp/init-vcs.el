;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-
;;; Code:

(use-package transient
  :ensure t
  ;; 可选：延迟加载，很多时候 magit 会触发加载，也可以去掉
  :demand t
  :init
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat sea-etc-dir "transient/levels")
	transient-values-file  (concat sea-etc-dir "transient/values")
	transient-history-file (concat sea-etc-dir "transient/values")
	transient-history-file (concat sea-etc-dir "transient/history"))
  )
  
;; Git
(use-package magit
  :defer t
  :after transient
  :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  )

;; (use-package magit-popup
;;   :defer t)

;; Gitflow externsion for Magit
;; (use-package magit-gitflow
;;   :after transient
;;   :diminish magit-gitflow-mode
;;   :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))


;;; Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :bind (("C-x v p" . git-messenger:popup-message)
	 :map git-messenger-map
	 ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t)

;; Highlighting regions by last updated time
(use-package smeargle
  :bind (("C-x v S" . smeargle)
	 ("C-x v C" . smeargle-commits)
	 ("C-x v R" . smeargle-clear)))



(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
