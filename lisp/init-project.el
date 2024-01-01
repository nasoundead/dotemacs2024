(require 'project)

;; (defun project-root-override (dir)
;;   "Find DIR's project root by searching for a '.project.el' file.

;; If this file exists, it marks the project root. For convenient compatibility
;; with Projectile, '.projectile' is also considered a project root marker.

;; https://blog.jmthornton.net/p/emacs-project-override"
;;   (let ((root (or (locate-dominating-file dir ".project.el")
;;                   (locate-dominating-file dir ".projectile")))
;;         (backend (ignore-errors (vc-responsible-backend dir))))
;;     (when root (if (version<= emacs-version "28")
;;                     (cons 'vc root)
;;                   (list 'vc backend root)))))

(defun project-root-override (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".project")
                      ("go.mod" "Cargo.toml" "pom.xml" "package.json") ;; higher priority
                      ("Makefile" "README.org" "README.md"))))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'local root))))))))

(setq project-find-functions '(project-root-override project-try-vc))


(define-key project-prefix-map (kbd "b") #'+project-blink-search)
(define-key project-prefix-map (kbd "m") #'+project-magit)
(define-key project-prefix-map (kbd "d") #'+project-dired)
(define-key project-prefix-map (kbd "f") #'project-find-file)
(define-key project-prefix-map (kbd "F") #'project-find-dir)
(define-key project-prefix-map (kbd "g") #'+project-rg)
(define-key project-prefix-map (kbd "t") #'multi-vterm-project)

(setq project-switch-commands nil)
(add-to-list 'project-switch-commands '(+project-blink-search "BlinkSearch") t)
(add-to-list 'project-switch-commands '(+project-magit "Magit") t)
(add-to-list 'project-switch-commands '(+project-dired "Dired") t)
(add-to-list 'project-switch-commands '(project-find-file "Find file") t)
(add-to-list 'project-switch-commands '(project-find-dir "Find dir") t)
(add-to-list 'project-switch-commands '(+project-rg "Ripgrep") t)
(add-to-list 'project-switch-commands '(multi-vterm-project "Vterm") t)

;; ;;;###autoload
;; (one-key-create-menu
;;   "Project"
;;   '(
;;     (("p" . "Switch project") . project-switch-project)
;;     (("f" . "Find file in project") . project-find-file)
;;     (("a". "Remmeber a proejct") . project-remember-projects-under)
;;     (("R" . "Remove known project") . project-forget-project)
;;     (("b" . "Project buffer") . consult-project-buffer)
;;     (("s" . "Project blink search") . +project-blink-search)
;;     (("r" . "Project rg") . +project-rg)
;;     (("e" . "Project eshell") . project-eshell)
;;     (("t" . "Project vterm") . multi-vterm-project)
;;     ))

;;;###autoload
(defun +project-rg ()
  (interactive)
  (let ((default-directory (project-root-override (project-current nil))))
    (consult-ripgrep default-directory)))

;;;###autoload
(defun +project-magit ()
  (interactive)
  (require 'magit)
  (magit-status (project-root-override (project-current nil))))

;;;###autoload
(defun +project-dired ()
  (interactive)
  (let ((default-directory (project-root-override (project-current nil))))
    (dired default-directory)))

;;;###autoload
(defun +project-blink-search ()
  (interactive)
  (let ((default-directory (project-root-override (project-current nil))))
    (require 'blink-search)
    (blink-search)))

;;;###autoload
(defun +project-recentf ()
  (interactive)
  (find-file (completing-read "Recentf in project"
		       (-filter (lambda (it)
				  (s-contains? (car (-take-last 1 (project-current))) it)) recentf-list)
		       )))


(provide 'init-project)