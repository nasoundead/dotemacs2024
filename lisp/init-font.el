(defvar sea-font-size 11
  "Current font size.")

(defvar sea-fonts '((default . "JetBrainsMono Nerd Font")
		    (cjk . "Microsoft Yahei")
		    (symbol . "Symbola")
		    (fixed . "Fira Code")
		    (fixed-serif . "Fira Code")
		    ;; (fixed-serif . "Monospace Serif")
		    (variable . "Fira Code")
		    ;; (variable . "Sans Serif")
		    (wide . "Fira Code")
		    ;; (wide . "Monospace")
		    (tall . "Fira Code"))
  ;; (tall . "Monospace"))
  "Fonts to use.")

(defun sea--get-font-family (key)
    (let ((font (alist-get key sea-fonts)))
	(if (string-empty-p font)
	    (alist-get 'default sea-fonts)
	font)))

(defun sea-load-default-font ()
    "Load default font configuration."
    (let ((default-font (format "%s-%s"
				(sea--get-font-family 'default)
				sea-font-size)))
	(add-to-list 'default-frame-alist (cons 'font default-font))))

(defun sea-load-face-font ()
"Load face font configuration."
(let ((variable-font (sea--get-font-family 'variable))
	(fixed-font (sea--get-font-family 'fixed))
	(fixed-serif-font (sea--get-font-family 'fixed-serif)))
    (set-face-attribute 'variable-pitch nil :family variable-font)
    (set-face-attribute 'fixed-pitch nil :family fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font)))

(defun sea-load-charset-font (&optional font)
    "Load charset font configuration."
    (let ((default-font (or font (format "%s-%s"
					    (sea--get-font-family 'default)
					    sea-font-size)))
	    (cjk-font (sea--get-font-family 'cjk))
	    (symbol-font (sea--get-font-family 'symbol)))
	(set-frame-font default-font)
	(dolist (charset '(kana han hangul cjk-misc bopomofo))
	(set-fontset-font t charset cjk-font))
	(set-fontset-font t 'symbol symbol-font)))

(sea-load-default-font)
(sea-load-face-font)

(defvar sea-font-current-variant nil)

(defun sea-dynamic-set-font (&rest ignore)
(interactive)
(when window-system
    (when (or (frame-root-window-p (get-buffer-window))
	    (frame-root-window-p (window-parent)))
    (let* ((prev-font-style sea-font-current-variant)
	    (wl (seq-filter (lambda (w) (not (string-prefix-p " " (buffer-name (window-buffer w))))) (window-list)))
	    (def (sea--get-font-family 'default))
	    (new-variant (cond
			    ((= 1 (length wl))
			    (sea--get-font-family 'default))

			    ((window-combined-p)
			    (sea--get-font-family 'tall))

			    (t
			    (sea--get-font-family 'wide)))))
    (unless (equal prev-font-style new-variant)
	(setq sea-font-current-variant new-variant)
	(set-frame-font new-variant)
	(sea-load-charset-font new-variant))))))

;; (setq frame-inhibit-implied-resize t)
(add-hook 'window-state-change-hook 'sea-dynamic-set-font)

;; Run after startup
(add-hook 'after-init-hook
	(lambda ()
	    (when window-system
	    (sea-load-charset-font))))


(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
				   all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
				      all-the-icons-faicon all-the-icons-fileicon
				      all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty))
    )

(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)

(provide 'init-font)
