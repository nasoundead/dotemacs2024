(use-package evil
  :straight (:host github
             :repo "emacs-evil/evil")
  :init
  (setq evil-magic 'very-magic)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))


(use-package evil-escape
  :straight (:host github
             :repo "syl20bnr/evil-escape")
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  :config
  (evil-escape-mode))

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-snipe
  :ensure t
  :diminish
  :init
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; (evil-define-key '(normal motion) evil-snipe-local-mode-map
  ;;   "s" nil
  ;;   "S" nil)
  (setq evil-snipe-scope 'whole-visible)
  (setq evil-snipe-repeat-scope 'whole-visible)

  (evil-define-key 'operator evil-snipe-local-mode-map
    "z" 'evil-snipe-s
    "Z" 'evil-snipe-S
    "x" 'evil-snipe-x
    "X" 'evil-snipe-X)

  (evil-define-key 'motion evil-snipe-override-local-mode-map
    "f" 'evil-snipe-f
    "F" 'evil-snipe-F
    "t" 'evil-snipe-t
    "T" 'evil-snipe-T)

  (when evil-snipe-override-evil-repeat-keys
    (evil-define-key 'motion map
      ";" 'evil-snipe-repeat
      "," 'evil-snipe-repeat-reverse)))
(use-package evil-surround
  :defer t
  :init
  (global-evil-surround-mode)
  :ensure t)
(use-package evil-args)
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))
(use-package evil-collection
  :init
  (add-hook 'after-init-hook  #'evil-collection-init)
  (evil-define-key 'normal dired-mode-map
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "=") 'sea/dired-diff
    "`" 'dired-open-term
    "o" 'dired-find-file-other-window
    "s" 'dired-sort-toggle-or-edit
    "z" 'dired-get-size
    ")" 'dired-omit-mode)

  (evil-define-key 'normal help-mode-map
    "o" 'link-hint-open-link)
  )


;; key	explanation
;; ------------------
;; gh, gj, gk, gl	navigate between elements
;; vae	select an element

;; Headings and items
;; ------------------
;; key	explanation
;; M-ret	insert heading
;; <tab>, g TAB	fold / unfold headings
;; M-h or <<	promote a heading
;; M-l or >>	demote a heading
;; M-k	move subtree up
;; M-j	move subtree down
;; M-S-h or <aR	promote a subtree
;; M-S-l or >aR	demote a subtree
;; vaR	select a subtree

;; Tables
;; ------------------
;; key	explanation
;; (	previous table cell
;; )	next table cell
;; {	beginning of table
;; }	end of table
;; M-h / M-l	move table column left / right
;; M-k / M-j	move table column up / down
;; vae	select table cell
;; vaE	select table row
;; var	select whole table

;; Agenda
;; ------------------
;; Evil key	Emacs key	explanation
;; gH		Move cursor to the top of window
;; gM		Move cursor to the middle of window
;; gL		Move cursor to the bottom of window
;; <tab>, S-<return>	<tab>	go to the corresponding entry at point
;; g TAB	<tab>	go to the corresponding entry at point
;; <return>	<return>	go to the Org mode file which contains the item at point
;; M-<return>	L	Display Org file and center around the item
;; <space>	<space>	scroll up
;; <delete> or <backspace>	<delete> or <backspace>	scroll down
;; j, k	n, p	next, previous line
;; gj, gk, C-j, C-k	N, P	next, previous item
;; [, ]	b, f	previous, next week
;; J, K	-, +, S-down, S-up	down, up priority
;; H, L	S-left, S-right	modify date to earlier, later
;; t	t	cycle TODO keywords
;; M-j, M-k	M-down, M-up	drag line forward, backward
;; C-S-h, C-S-l	C-S-left, C-S-right	previous, next keyword
;; u	C-_, C-/	undo
;; dd	C-k	delete item
;; da	a	ask and archive item
;; dA	$	archive item
;; ct	:	set tags
;; ce	e	set effort
;; cT	;	set timer
;; i	i	insert entry in diary
;; a	z	add note
;; A	A	append to agenda
;; C	k	capture
;; m	m	mark
;; *	*	toggle all marks
;; %	%	mark regexp
;; M	U	remove all marks
;; x	B	execute action on marks
;; gr	r	refresh agenda
;; gR	g	refresh all agendas
;; ZQ	x	exit agenda
;; ZZ	Q	quit agenda
;; gD	v	tweak display (deadlines, diary, follow/log-mode, entry text, grid, day/week/year
;; ZD	#	dim blocked tasks
;; sc, sr, se, st, s^	<, =, _, /, ^	filter by category, regexp, effort, tag, top headline
;; S	|	remove all filters
;; ss	~	filter/limit interactively
;; I	I	clock in
;; O	O	clock out
;; cg	J	jump to the currently clocked in task within the agenda
;; cc	X	cancel the current running clock
;; cr	R	toggle clocktable mode in an agenda buffer
;; .	.	go to today’s date
;; gc	c	pop up calendar
;; gC	C	pop up date converter
;; p	>	pop up date selector
;; gh	H	pop up holiday calendar
;; gm	M	pop up phases of the moon
;; gs	S	pop up sunrise/sunset times
;; gt	T	pop up tag list
;; +, -	[, ]	manipulate the query by adding a search term with positive or negative selection
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'init-evil)
