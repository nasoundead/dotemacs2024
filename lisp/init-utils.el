;; (use-package fanyi
;;   :ensure t
;;   :custom
;;   (fanyi-providers '(;; 海词
;;		     ;; fanyi-haici-provider
;;		     ;; 有道同义词词典
;;		     fanyi-youdao-thesaurus-provider
;;		     ;; Etymonline
;;		     fanyi-etymon-provider
;;		     ;; Longman
;;		     ;; fanyi-longman-provider)
;;		     )))


;; (use-package avy
;;   :ensure t
;;   :bind (("C-;" . avy-goto-char)))

;; (use-package ace-pinyin
;;   :straight (ace-pinyin
;;               :type git
;;               :host github
;;               :repo "cute-jumper/ace-pinyin")
;;   :config
;;   (ace-pinyin-global-mode +1))

(use-package bing-dict
  :ensure t
  :bind ("C-c f" . bing-dict-brief))

(defun sea/revert-auto-encoding ()
  "自动检测当前文件编码并重新读取，标记为 UTF-8 保存。"
  (interactive)
  (let* ((raw (with-temp-buffer
		(set-buffer-multibyte nil)
		(insert-file-contents-literally (buffer-file-name))
		(buffer-string)))
	 (coding (detect-coding-string raw t)))
    (revert-buffer-with-coding-system coding)
    (set-buffer-file-coding-system 'utf-8)
    (message "检测到编码 %s，下次保存将转为 UTF-8" coding)))

(defun sea/fix-org-encoding-buffer ()
  "修复当前 Org 文件：中文星期→英文 + 清除 ^M + 平台行尾。"
  (interactive)
  (let ((weekday-map '(("周一" . "Mon") ("周二" . "Tue") ("周三" . "Wed")
		       ("周四" . "Thu") ("周五" . "Fri") ("周六" . "Sat")
		       ("周日" . "Sun")))
	(file (buffer-file-name)))
    ;; 探测 raw 字节，有则用 GBK 重读
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "[\200-\377]" nil t)
	(revert-buffer-with-coding-system 'chinese-gbk)))
    ;; 替换 date 和 last_modified 中的中文星期为英文
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(#\\+date:\\|#\\+last_modified:\\)" nil t)
	(let ((end (line-end-position)))
	  (dolist (pair weekday-map)
	    (save-excursion
	      (while (search-forward (car pair) end t)
		(replace-match (cdr pair))))))))
    ;; 修复 last_modified 为当前时间
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified: *\\(.*\\)$" nil t)
	(let ((val (match-string 1)))
	  (when (or (string-blank-p val)
		    (string-match-p "[\200-\377]" val))
	    (delete-region (match-beginning 1) (match-end 1))
	    (goto-char (match-beginning 1))
	    (insert (format-time-string "[%Y-%m-%d %a %H:%M]"))))))
    ;; 清除 date/last_modified 后多余空行（最多保留一个）
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(#\\+date:\\|#\\+last_modified:\\)" nil t)
	(forward-line)
	(when (looking-at "\n\n+")
	  (delete-region (match-beginning 0) (1- (match-end 0))))))
    ;; 清除所有 ^M
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r" nil t)
	(replace-match "")))
    ;; 平台行尾
    (set-buffer-file-coding-system (if sys/winp 'utf-8-dos 'utf-8-unix))
    (message "修复完成，请保存 (C-x C-s)")))

(defun sea/fix-org-batch ()
  "全量修复 ~/org/ 下所有 .org 文件：中文星期→英文 + 清除 ^M + 平台行尾。"
  (interactive)
  (let ((weekday-map '(("周一" . "Mon") ("周二" . "Tue") ("周三" . "Wed")
		       ("周四" . "Thu") ("周五" . "Fri") ("周六" . "Sat")
		       ("周日" . "Sun")))
	(files (directory-files-recursively "~/org/" "\\.org$"))
	(fixed 0))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
	;; GBK 重读
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward "[\200-\377]" nil t)
	    (revert-buffer-with-coding-system 'chinese-gbk)
	    (set-buffer-file-coding-system
	     (if sys/winp 'utf-8-dos 'utf-8-unix))))
	;; 替换中文星期为英文
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(#\\+date:\\|#\\+last_modified:\\)" nil t)
	    (let ((end (line-end-position)))
	      (dolist (pair weekday-map)
		(save-excursion
		  (while (search-forward (car pair) end t)
		    (replace-match (cdr pair))))))))
	;; 修复 last_modified
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward "^#\\+last_modified: *\\(.*\\)$" nil t)
	    (let ((val (match-string 1)))
	      (when (or (string-blank-p val)
			(string-match-p "[\200-\377]" val))
		(delete-region (match-beginning 1) (match-end 1))
		(goto-char (match-beginning 1))
		(insert (format-time-string "[%Y-%m-%d %a %H:%M]"))))))
	;; 清除 date/last_modified 后多余空行
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(#\\+date:\\|#\\+last_modified:\\)" nil t)
	    (forward-line)
	    (when (looking-at "\n\n+")
	      (delete-region (match-beginning 0) (1- (match-end 0))))))
	;; 清除 ^M
	(save-excursion
	  (goto-char (point-min))
	  (while (search-forward "\r" nil t)
	    (replace-match "")))
	;; 保存
	(set-buffer-file-coding-system (if sys/winp 'utf-8-dos 'utf-8-unix))
	(save-buffer)
	(kill-buffer)
	(setq fixed (1+ fixed))))
    (message "已修复 %d 个文件" fixed)))

(defun sea/clean-auto-save-files ()
  "清空当前项目中的 #xxx# 和 .#xxx 临时文件。"
  (interactive)
  (let* ((root (or (projectile-project-root) default-directory))
	 (files (directory-files-recursively root "\\`[#\\.]#"))
	 (count 0))
    (dolist (f files)
      (delete-file f)
      (setq count (1+ count)))
    (message "已删除 %d 个临时文件" count)))

(defun sea/fix-el-eol ()
  "修复 ~/.emacs.d 下自有 .el 文件的行尾：清除 ^M，转为平台行尾。

跳过 straight/ elpa/ 等第三方仓库目录。"
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory))
        (skip-dirs '("straight" "elpa" "eln-cache" ".cache" "auto-save-list"))
        (fixed 0))
    (dolist (file (directory-files-recursively dir "\\.el$"))
      (unless (cl-some (lambda (d)
                         (string-match-p (concat "[/\\\\]" d "[/\\\\]") file))
                       skip-dirs)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (search-forward "\r" nil t)
              (replace-match "")))
          (set-buffer-file-coding-system
           (if sys/winp 'utf-8-dos 'utf-8-unix))
          (save-buffer)
          (kill-buffer)
          (setq fixed (1+ fixed)))))
    (message "已修复 %d 个 .el 文件" fixed)))

(provide 'init-utils)
