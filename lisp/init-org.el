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
    ("[ ]" . "☐")
    ("[-]" . "🝕")
    ("[X]" . "🗹")
    ("#+BEGIN_SRC"    . ?⌜)
    ("#+END_SRC"      . ?⌞)
    ("#+begin_src"    . ?⌜)
    ("#+end_src"      . ?⌞)
    ;; ("#+BEGIN_SRC" . "✎")
    ;; ("#+END_SRC" . "□")
    ;; ("#+begin_src" . "✎")
    ;; ("#+end_src" . "□")

    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+begin_quote"  . ?«)
    ("#+end_quote"    . ?»)

    ("#+begin_verse" . "ζ")
    ("#+end_verse" . "□")
    ("#+BEGIN_VERSE" . "ζ")
    ("#+END_VERSE" . "□")

    ("#+BEGIN_EXAMPLE" . "⟝")
    ("#+END_EXAMPLE" . "□")
    
    ("#+BEGIN_EXPORT" . "🙵")
    ("#+END_EXPORT" . "□")
    ("#+RESULTS:"     . ?💻)
    ;; ("#+RESULTS:" . "⟾")
    ("#+CAPTION:" . "✑")
    ("#+ATTR_LATEX" . "🄛"))
    "sea-prettify-org-symbols-alist")

;; 确保正文跟随标题缩进的核心配置
(setq org-startup-indented t)  ; 启动时自动启用缩进模式

;; 强制设置正文与标题的缩进关系（每级标题的正文额外缩进）
(setq org-indent-indentation-per-level 2)  ; 每级缩进 2 空格
(setq org-indent-text-line-function 'org-indent-text-line)  ; 正文缩进函数

;; 禁用可能干扰缩进的设置
(setq org-adapt-indentation nil)  ; 不自动调整缩进适应内容

  ;; Babel
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(defconst load-language-alist
    '((emacs-lisp . t)
      (python     . t)
      (js         . t)
      (css        . t)
      (C          . t)
      (java       . t)
      (plantuml   . t)
      )
    "Alist of org ob languages.")
;; ob-sh renamed to ob-shell since 26.1.
(cl-pushnew '(shell . t) load-language-alist)
(use-package ob-ipython
  :init (cl-pushnew '(ipython  . t) load-language-alist))
(use-package ob-go
  :init (cl-pushnew '(go . t) load-language-alist))
(use-package ob-rust
  :init (cl-pushnew '(rust . t) load-language-alist))
(use-package plantuml-mode
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
(org-babel-do-load-languages 'org-babel-load-languages load-language-alist)

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
(use-package org-superstar
  :custom
  ;; org-superstar-headline-bullets-list '("⦿" "⌾" "⊚" "𐰧" "►" "▻")
  ;; org-superstar-headline-bullets-list '("⦿" "⌾" "⊚" "🞅" "▸" "▹")
  ;; org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ")
  org-superstar-prettify-item-bullets nil
  :hook (org-mode . org-superstar-mode))

(use-package xeft
  :config
  (setq xeft-default-extension "org")
  (setq xeft-directory "~/org/")
  (setq xeft-ignore-extension '("png"))
  (setq xeft-title-function #'file-name-nondirectory)
  ;; Follow symlinks.
  (setq xeft-recursive 'follow-symlinks)
  (defvar-local xeft--displayed-by-xeft-p nil)

  (defun xeft--eager-preview()
    (when-let* ((button (button-at (point)))
    (path (button-get button 'path)))
    ;; Kill previously displayed buffer.
    (when (window-live-p xeft--preview-window)
      (with-selected-window xeft--preview-window
        (when xeft--displayed-by-xeft-p
          (kill-buffer))))
    ;; Show preview of current selection.
    (xeft--preview-file path)))

  (add-hook 'xeft-find-file-hook
      (lambda () (setq xeft--displayed-by-xeft-p t)))

  (advice-add 'xeft-next :after #'xeft--eager-preview)
  (advice-add 'xeft-previous :after #'xeft--eager-preview))


(use-package emacsql)
  ;; (use-package emacsql-sqlite)
  ;; (require 'emacsql-sqlite)
(use-package org-roam
    :after org
    ;; :init
    ;; (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
    :custom
    ;; (org-roam-database-connector 'sqlite-builtin)
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
          "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
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
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

    (defun pv/org-set-last-modified ()
      "Update the LAST_MODIFIED file property in the preamble."
      (when (derived-mode-p 'org-mode)
	      (pv/org-set-time-file-property "last_modified")))

    :hook
    (before-save . pv/org-set-last-modified))

;; 批量刷新 Org-roam 笔记编码并重新保存
(defun org-roam-refresh-all-files-encoding ()
    "强制所有 Org-roam 笔记以 UTF-8 编码重新保存，解决 DATE 字段编码问题"
    (interactive)
    (let ((files (org-roam-list-files)))  ; 获取所有 Org-roam 笔记文件
      (dolist (file files)
	(when (file-exists-p file)
	  (with-current-buffer (find-file-noselect file)
	    ;; 强制设置编码为 UTF-8
	    (set-buffer-file-coding-system 'utf-8-unix)
	    ;; 保存文件（不打开窗口）
	    (save-buffer)
	    (kill-buffer))))
      (message "Org-roam 所有文件已按 UTF-8 重新编码并保存")))

(use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))



(use-package org-download
  :after org
  :defer nil
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

	  )))


;; (use-package org-modern
;;   ;; :custom
;;   ;; Org modern settings
;;   ;; (org-modern-star nil)
;;   ;; (org-modern-priority nil)
;;   ;; (org-modern-list nil)
;;   ;; (org-modern-checkbox nil)
;;   ;; (org-modern-todo nil)
;;   ;; (org-modern-keyword nil)

;;   ;; Editor settings
;;   ;; (org-auto-align-tags nil)
;;   ;; (org-tags-column 0)
;;   ;; (org-catch-invisible-edits 'show-and-error)
;;   ;; (org-special-ctrl-a/e t)
;;   :config
;;   ;; (global-org-modern-mode 1)
;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(provide 'init-org)
