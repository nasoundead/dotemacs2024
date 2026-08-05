;;; init-org.el
;;; Code:

(straight-use-package 'org)


(defconst sea-org-directory
  (expand-file-name "~/org/")
  "org dir")

(defconst sea-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "sea-prettify-symbols-alist")

(defconst sea-prettify-org-symbols-alist
  '(
    ("#+BEGIN_SRC"    . ?⌜)
    ("#+END_SRC"      . ?⌞)
    ("#+begin_src"    . ?⌜)
    ("#+end_src"      . ?⌞)

    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+begin_quote"  . ?«)
    ("#+end_quote"    . ?»)

    ("#+begin_verse" . "ζ")
    ("#+end_verse" . "□")
    ("#+BEGIN_VERSE" . "ζ")
    ("#+END_VERSE" . "□")

    ("#+RESULTS:"     . ?💻)
    ("#+CAPTION:" . "✑")
    ("#+ATTR_LATEX" . "🄛"))
  "sea-prettify-org-symbols-alist")

(defconst load-language-alist
  '((emacs-lisp . t)
    (python     . t)
    (js         . t)
    (css        . t)
    (C          . t)
    (java       . t)
    (plantuml   . t)
    (shell      . t)
    (calc       . t)
    )
  "Alist of org ob languages.")
(use-package plantuml-mode
  :after org
  :init
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; Integration with org-mode
  (cl-pushnew '(plantuml . t) load-language-alist)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  :config
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" sea-etc-dir))
  (defun sea/plantuml-install()
    (let ((url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
      (unless (file-exists-p org-plantuml-jar-path)
	(url-copy-file url org-plantuml-jar-path))))
  (add-hook 'org-mode-hook #'(lambda () (eval-after-load 'ob-plantuml (sea/plantuml-install)))))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-switchb)
	 ("C-c x" . org-capture)
	 :map org-mode-map
	 ("C-c C-e" . org-emphasize))
  :hook ((org-babel-after-execute org-mode) . org-redisplay-inline-images)
  :custom-face
  (org-document-title ((t (:weight bold :height 1.2 :underline nil))))
  (org-level-1 ((t (:weight bold :height 1.25))))
  (org-level-2 ((t (:weight bold :height 1.15))))
  (org-level-3 ((t (:weight bold :height 1.12))))
  (org-level-4 ((t (:weight bold :height 1.09))))
  (org-level-5 ((t (:weight bold :height 1.06))))
  (org-level-6 ((t (:weight bold :height 1.03))))
  (org-level-7 ((t (:weight bold))))
  (org-level-8 ((t (:weight bold))))
  :config
  ;; 确保正文跟随标题缩进的核心配置
  (setq org-startup-indented t)  ; 启动时自动启用缩进模式
  ;; 启动时自动显示内联图片
  (setq org-startup-with-inline-images t)
  (setq org-hide-leading-stars t)
  ;; 全局强制英文星期，杜绝中文编码问题
  (setq system-time-locale "C")
  ;; org 文件保存时不再询问编码，强制定为 UTF-8
  (defun sea/org-force-utf-8 ()
    (when (derived-mode-p 'org-mode)
      (set-buffer-file-coding-system
       (if sys/winp 'utf-8-dos 'utf-8-unix))))
  (add-hook 'org-mode-hook #'sea/org-force-utf-8)
  (add-hook 'before-save-hook #'sea/org-force-utf-8)
  ;; 强制设置正文与标题的缩进关系（每级标题的正文额外缩进）
  (setq org-indent-indentation-per-level 2)  ; 每级缩进 2 空格
  (setq org-indent-text-line-function 'org-indent-text-line)  ; 正文缩进函数
  ;; 禁用可能干扰缩进的设置
  (setq org-adapt-indentation t)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)"))
	org-todo-keyword-faces '(("HANGUP" . warning)))
  ;; Babel
  (setq org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-tab-acts-natively t)
  (org-babel-do-load-languages 'org-babel-load-languages load-language-alist)
  ;; 隐藏 emphasis 标记符 (* / _ + = ~)，只显示样式
  (setq org-hide-emphasis-markers t)
  ;; Make verbatim with highlight text background.
  (add-to-list 'org-emphasis-alist
	       '("=" (:background "#fef7ca")))
  ;; Make deletion(obsolote) text foreground with dark gray.
  (add-to-list 'org-emphasis-alist
	       '("+" (:foreground "dark gray"
				  :strike-through t)))
  ;; Make code style around with box.
  (add-to-list 'org-emphasis-alist
	       '("~" (:box (:line-width 1
					:color "grey75"
					:style released-button))))
  )

;; Rich text clipboard
(use-package org-rich-yank
  :bind (:map org-mode-map
	  ("C-M-y" . org-rich-yank)))
(use-package valign
  :custom (valign-fancy-bar t)
  :hook (org-mode . valign-mode))

;; Table of contents
(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; Auto-toggle Org LaTeX fragments
(use-package org-fragtog
  :diminish
  :hook (org-mode . org-fragtog-mode))

;; Preview
(use-package org-preview-html
  :diminish
  :bind (:map org-mode-map
	  ("C-c C-h" . org-preview-html-mode))
  :init (when (featurep 'xwidget-internal)
	  (setq org-preview-html-viewer 'xwidget)))

;; Presentation
(use-package org-tree-slide
  :diminish
  :functions (org-display-inline-images
	      org-remove-inline-images)
  :bind (:map org-mode-map
	  ("s-<f7>" . org-tree-slide-mode)
	  :map org-tree-slide-mode-map
	  ("<left>" . org-tree-slide-move-previous-tree)
	  ("<right>" . org-tree-slide-move-next-tree)
	  )
  :hook
  ((org-tree-slide-play . (lambda ()
			    (text-scale-increase 4)
			    (org-display-inline-images)
			    (read-only-mode 1)))
   (org-tree-slide-stop . (lambda ()
			    (text-scale-increase 0)
			    (org-remove-inline-images)
			    (read-only-mode -1))))
  :init
  (setq org-tree-slide-header nil
	org-tree-slide-slide-in-effect t
	org-tree-slide-heading-emphasis nil
	org-tree-slide-cursor-init t
	org-tree-slide-progress-bar t
	org-tree-slide-modeline-display 'outside
	org-tree-slide-skip-done nil
	org-tree-slide-skip-comments t
	org-tree-slide-skip-outline-level 3))
;;;; org-superstar
;; (use-package org-superstar
;;   :custom
;;   ;; org-superstar-headline-bullets-list '("⦿" "⌾" "⊚" "𐰧" "►" "▻")
;;   ;; org-superstar-headline-bullets-list '("⦿" "⌾" "⊚" "🞅" "▸" "▹")
;;   ;; org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ")
;;   org-superstar-headline-bullets-list '("⦿" "⌾" "⊚" "🞅" "▸" "▹")
;;   ;; org-superstar-prettify-item-bullets nil
;;   :hook (org-mode . org-superstar-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-heading-numbers t)
  ;; (org-modern-star ["⦿" "⌾" "⊚" "🞅" "▸" "▹"])
  (org-modern-hide-stars t)
  )

;; (use-package org-visual-outline
;;   :straight (org-visual-outline :type git :host github :repo "legalnonsense/org-visual-outline")
;;   :hook (org-mode . (lambda ()
;;		      (org-visual-outline-mode)
;;		      (org-dynamic-bullets-mode)
;;		      (org-visual-indent-mode)))
;;   :config
;;   (setq org-visual-indent-color-indent '((1 (:background "blue" :foreground "blue" :height .1))
;;					 (2 (:background "red" :foreground "red" :height .1))
;;					 (3 (:background "green" :foreground "green" :height .1))))
;;   (setq org-visual-indent-color-indent
;;	(cl-loop for x from 1 to 8
;;		 with color = nil
;;		 do (setq color (or (face-foreground
;;				     (intern
;;				      (concat "org-level-"
;;					      (number-to-string x))))
;;				    (face-foreground 'org-level-1)))
;;		 collect `(,x ,(list
;;				:background color
;;				:foreground color
;;				:height .1)))))

(use-package org-roam
  :after org
  :commands (org-roam-refresh-all-files-encoding)
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-dailies-directory "daily/") ;; 默认日记目录, 上一目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  (org-roam-directory "~/org/roam/") ; 设置 org-roam 目录
  (org-time-stamp-formats
   '("<%Y-%m-%d %a %H:%M>" . "<%Y-%m-%d %a %H:%M>"))
  ;; 自定义默认模板
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new
      (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
		 "#+title: ${title}\n#+date: %(let ((system-time-locale \"C\")) (format-time-string \"[%Y-%m-%d %a %H:%M]\"))\n#+last_modified: \n\n")
      :immediate-finish t)))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n c" . org-roam-capture)
   ("C-c n d" . org-roam-dailies-map)
   ("C-c n u" . org-roam-ui-mode))
  :config
  (org-roam-setup)
  ;;--------------------------
  ;; Handling file properties for ‘LAST_MODIFIED’
  ;;--------------------------
  (defun pv/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
      When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
	     (save-excursion
	       (re-search-forward org-outline-regexp-bol nil t))))
	(when (re-search-forward (format "^#\\+%s:" property)
				 (if anywhere nil first-heading)
				 t)
	  (point)))))

  (defun pv/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
	As a special case, return -1 if the time file PROPERTY exists but
	is not defined."
    (when-let ((pos (pv/org-find-time-file-property property anywhere)))
      (save-excursion
	(goto-char pos)
	(if (and (looking-at-p " ")
		 (progn (forward-char)
			(org-at-timestamp-p 'lax)))
	    pos
	  -1))))
  (defun pv/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.

	When ANYWHERE is non-nil, search beyond the preamble.

	If the position of the file PROPERTY has already been computed,
	it can be passed in POS."
    (when-let ((pos (or pos
			(pv/org-find-time-file-property property))))
      (save-excursion
	(goto-char pos)
	(if (looking-at-p " ")
	    (forward-char)
	  (insert " "))
	(delete-region (point) (line-end-position))
	(let* ((now (let ((system-time-locale "C"))
		      (format-time-string "[%Y-%m-%d %a %H:%M]"))))
	  (insert now)))))

  (defun pv/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (pv/org-set-time-file-property "last_modified")))

  (defun org-roam-refresh-all-files-encoding ()
    "Fix date fields in all Org-roam files."
    (interactive)
    (dolist (file (org-roam-list-files))
      (when (file-exists-p file)
	(with-current-buffer (find-file-noselect file)
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "^#\\+date:" nil t)
	      (delete-region (line-beginning-position) (line-end-position))
	      (let ((s (let ((system-time-locale "C"))
			 (format-time-string "[%Y-%m-%d %a %H:%M]"
					     (file-attribute-modification-time
					      (file-attributes file))))))
		(insert "#+date: " s)))
	    (goto-char (point-min))
	    (when (re-search-forward "^#\\+last_modified:" nil t)
	      (delete-region (line-beginning-position) (line-end-position))
	      (insert "#+last_modified: ")))
	  (set-buffer-file-coding-system 'utf-8)
	  (save-buffer)
	  (kill-buffer)))))
  (message "Org-roam encoding fix done")

  (add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook #'pv/org-set-last-modified nil t)))
  )


(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))


(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (when (eq system-type 'windows-nt)
    (setq org-download-screenshot-method "convert clipboard: %s"))
  (org-download-annotate-function 'ignore)
  ;; :bind
  ;; ("C-M-y" . org-download-screenshot)
  :bind (:map org-mode-map
	  ("<f2>" . org-download-clipboard))
  :config
  (require 'org-download))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
	org-appear-autosubmarkers t
	org-appear-autoentities t
	org-appear-autolinks nil))

(use-package ace-pinyin
  :defer t
  :config
  (ace-pinyin-global-mode +1))

(use-package svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :config
  (defun mk/svg-checkbox-empty()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill 'white :rx 2 :stroke-width 2.5 :stroke-color 'black)
      (svg-image svg :ascent 'center)
      ))

  (defun mk/svg-checkbox-filled()
    (let* ((svg (svg-create 14 14)))
      (svg-rectangle svg 0 0 14 14 :fill "#FFFFFF" :rx 2)
      (svg-polygon svg '((5.5 . 11) (12 . 3.5) (11 . 2) (5.5 . 9) (1.5 . 5) (1 . 6.5))
		   :stroke-color 'black :stroke-width 1 :fill 'black)
      (svg-image svg :ascent 'center)
      ))
  (defun mk/svg-checkbox-toggle()
    (interactive)
    (save-excursion
      (let* ((start-pos (line-beginning-position))
	     (end-pos (line-end-position))
	     (text (buffer-substring-no-properties start-pos end-pos))
	     (case-fold-search t)  ; Let X and x be the same in search
	     )
	(beginning-of-line)
	(cond ((string-match-p "\\[X\\]" text)
	       (progn
		 (re-search-forward "\\[X\\]" end-pos)
		 (replace-match "[ ]")))
	      ((string-match-p "\\[ \\]" text)
	       (progn
		 (search-forward "[ ]" end-pos)
		 (replace-match "[X]")))
	      ))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
      (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-action-at-point 'edit)

  (setq svg-lib-icon-collections
	`(("bootstrap" .
	   "https://icons.getbootstrap.com/assets/icons/%s.svg")
	  ("simple" .
	   "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
	  ("material" .
	   "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
	  ("octicons" .
	   "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
	  ("boxicons" .
	   "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

  (setq svg-tag-tags
	`(
	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ( (lambda (tag)
				(svg-tag-make tag :face 'org-priority
					      :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; Checkbox
	  ("\\[ \\]" . ((lambda (_tag) (mk/svg-checkbox-empty))
			(lambda () (interactive) (mk/svg-checkbox-toggle))
			"Click to toggle."
			))
	  ("\\(\\[[Xx]\\]\\)" . ((lambda (_tag) (mk/svg-checkbox-filled))
				 (lambda () (interactive) (mk/svg-checkbox-toggle))
				 "Click to toggle."))

	  ;; Active date (with or without day name, with or without time)
	  (,(format "\\(<%s>\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0))))
	  (,(format "\\(<%s \\)%s>" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
	  (,(format "<%s \\(%s>\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

	  ;; Inactive date  (with or without day name, with or without time)
	  (,(format "\\(\\[%s\\]\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
	  (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
	  (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

	  ;; Keywords
	  ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-todo :margin 0 :radius 5))))
	  ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
						 :face 'org-todo :margin 0 :radius 5))))
	  ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
						 :face 'org-done :margin 0 :radius 5))))

	  ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))

	  ;; beautify pagebreak in orgmode
	  ("\\\\pagebreak" . ((lambda (tag) (svg-lib-icon "file-break" nil :collection "bootstrap"
							  :stroke 0 :scale 1 :padding 0))))

	  ))
  )

(provide 'init-org)
