;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "V")

(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      [remap describe-bindings] #'embark-bindings

      ;; treemacs
      (:after treemacs
	(:map treemacs-mode-map
	  :n "h" #'treemacs-goto-parent-node
	  :n "l" #'treemacs-RET-action
	  :n "d" #'treemacs-delete-file
	  :n "R" #'treemacs-rename-file
	  :n "cf" #'treemacs-create-file
	  :n "cd" #'treemacs-create-dir
	  :n "ya" #'treemacs-copy-absolute-path-at-point
	  :n "yr" #'treemacs-copy-relative-path-at-point
	  :n "yp" #'treemacs-copy-project-path-at-point
	  :n "yf" #'treemacs-copy-file
	  :n "q" #'treemacs-quit
	  :n "M-j" #'treemacs-next-neighbour
	  :n "M-k" #'treemacs-previous-neighbour
	  ))

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command

      ;; A little sandbox to run code in
      :gnvime "M-:" #'eval-expression

      ;; Text-scaling
      :n "M-="   #'text-scale-increase
      :n "M--"   #'text-scale-decrease

      :enm "C-h"   #'evil-window-left
      :enm "C-j"   #'evil-window-down
      :enm "C-k"   #'evil-window-up
      :enm "C-l"   #'evil-window-right

      ;; flycheck
      :n  "]e" #'next-error
      :n  "[e" #'previous-error

      ;; hl-todo
      :n  "]t" #'hl-todo-next
      :n  "[t" #'hl-todo-previous

      ;; --- personal vim-esque bindings ------------------
      :n  "zx" #'kill-this-buffer
      :m  "]a" #'evil-forward-arg
      :m  "[a" #'evil-backward-arg
      :n  "H" #'next-buffer
      :n  "L" #'previous-buffer
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :nv "gd" #'+lookup/definition
      :nv "gr" #'+lookup/references
      :nv "gi" #'+lookup/implementations
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent    ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent    ; vnoremap > >gv

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      (:map xref--xref-buffer-mode-map
	:n "RET" #'xref-goto-xref
	:n "j" #'xref-next-line
	:n "k" #'xref-prev-line)

      (:map custom-theme-choose-mode-map
	:gvnime "j" #'widget-forward
	:gvnime "k" #'widget-backward)

      (:map blink-search-mode-map
	:gvnime "C-j" #'blink-search-candidate-select-next
	:gvnime "C-k" #'blink-search-candidate-select-prev
	:gvnime "C-S-j" #'blink-search-candidate-group-select-next
	:gvnime "C-S-k" #'blink-search-candidate-group-select-prev
	:gvnime "M-j" #'blink-search-backend-select-next
	:gvnime "M-k" #'blink-search-backend-select-prev
	:gvnime "C-SPC" #'blink-search-preview
	:gvnime "C-h" #'blink-search-parent)

      (:after evil
	:textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr
	:textobj "a" #'evil-inner-arg                    #'evil-outer-arg
	:textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
	:textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
	:textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
	:textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

	(:map evil-window-map           ; prefix "C-w"
	  ;; Navigation
	  "C-h"     #'evil-window-left
	  "C-j"     #'evil-window-down
	  "C-k"     #'evil-window-up
	  "C-l"     #'evil-window-right
	  "C-w"     #'other-window
	  "C-S-w"   #'ace-swap-window
	  ;; Window undo/redo
	  "u"       #'winner-undo
	  "C-u"     #'winner-undo
	  "C-r"     #'winner-redo
	  ;; split
	  "s"       #'split-window-horizontally-instead
	  "v"       #'split-window-vertically-instead
	  ;; Delete window
	  "c"     #'delete-window
	  "h"     #'shrink-window-horizontally
	  "l"     #'enlarge-window-horizontally
	  "j"     #'enlarge-window
	  "k"     #'shrink-window
	  ))

      ;; evil-commentary
      :n  "gc"  #'evil-commentary

      ;; evil-exchange
      :n  "gx"  #'evil-exchange

      ;; evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit

      ;; evil-lion
      :n "gl" #'evil-lion-left
      :n "gL" #'evil-lion-right
      :v "gl" #'evil-lion-left
      :v "gL" #'evil-lion-right

      ;; evil-mc
      (:prefix "gz"
	:nv "m" #'evil-mc-make-all-cursors
	:nv "u" #'evil-mc-undo-all-cursors
	:nv "z" #'+evil/mc-make-cursor-here
	:nv "t" #'+evil/mc-toggle-cursors
	:nv "n" #'evil-mc-make-and-goto-next-cursor
	:nv "p" #'evil-mc-make-and-goto-prev-cursor
	:nv "N" #'evil-mc-make-and-goto-last-cursor
	:nv "P" #'evil-mc-make-and-goto-first-cursor
	:nv "d" #'evil-mc-make-and-goto-next-match
	:nv "D" #'evil-mc-make-and-goto-prev-match
	:nv "j" #'evil-mc-make-cursor-move-next-line
	:nv "k" #'evil-mc-make-cursor-move-prev-line)
      (:after evil-mc
	:map evil-mc-key-map
	:nv "C-n" #'evil-mc-make-and-goto-next-cursor
	:nv "C-N" #'evil-mc-make-and-goto-last-cursor
	:nv "C-p" #'evil-mc-make-and-goto-prev-cursor
	:nv "C-P" #'evil-mc-make-and-goto-first-cursor)

      ;; evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
	(:map evil-multiedit-state-map
	  "M-d" #'evil-multiedit-match-and-next
	  "M-D" #'evil-multiedit-match-and-prev
	  "RET" #'evil-multiedit-toggle-or-restrict-region)
	(:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
	  "C-n" #'evil-multiedit-next
	  "C-p" #'evil-multiedit-prev))

      ;; auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create
      ;; undo-tree


      ;; flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
      (:after flyspell
	;; Press RET on misspelled words to correct them
	(:map flyspell-mouse-map
	  "RET" #'flyspell-correct-word-generic
	  "<mouse-1>" #'flyspell-correct-word-generic))

      ;; git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      ;; git-timemachine
      (:after git-timemachine
	(:map git-timemachine-mode-map
	  :n "C-p" #'git-timemachine-show-previous-revision
	  :n "C-n" #'git-timemachine-show-next-revision
	  :n "C-k" #'git-timemachine-show-previous-revision
	  :n "C-j" #'git-timemachine-show-next-revision
	  :n "[["  #'git-timemachine-show-previous-revision
	  :n "]]"  #'git-timemachine-show-next-revision
	  :n "q"   #'git-timemachine-quit
	  :n "gb"  #'git-timemachine-blame))

      ;; gist
      (:after gist
	:map gist-list-menu-mode-map
	:n "b"   #'gist-browse-current-url
	:n "c"   #'gist-add-buffer
	:n "d"   #'gist-kill-current
	:n "f"   #'gist-fork
	:n "q"   #'quit-window
	:n "r"   #'gist-list-reload
	:n "s"   #'gist-star
	:n "S"   #'gist-unstar
	:n "y"   #'gist-print-current-url)

      ;; --- Built-in plugins -----------------------------
      (:map* (help-mode-map helpful-mode-map)
	     :n "o"  #'ace-link-help
	     :n "q"  #'quit-window
	     :n "]l" #'forward-button
	     :n "[l" #'backward-button)
      (:after vc-annotate
	:map vc-annotate-mode-map
	[remap quit-window] #'kill-this-buffer)
      )

;; <leader>
;;
(map! :leader
      :desc "M-x"                     :n "SPC" #'execute-extended-command
      :desc "Pop up scratch buffer"   :nv "x"  #'sea/open-scratch-buffer
      :desc "Org Capture"             :nv "X"  #'org-capture
      :desc "Blink Search"            :nv "."  #'+project-blink-search

      ;; C-u is used by evil
      :desc "Universal argument"      :n "u"  #'universal-argument
      :desc "window"                  :n "w"  #'evil-window-map


      (:desc "quit" :prefix "q"
      :desc "Quit Emacs"             :n "q" #'evil-quit-all
      :desc "Save and quit"          :n "Q" #'evil-save-and-quit
      :desc "Restart Doom"           :n "R" #'restart-emacs)

      (:desc "help" :prefix "h"
      :n "h" help-map
      :desc "Apropos"               :n  "a" #'apropos
      :desc "Describe char"         :n  "c" #'describe-char
      :desc "Describe function"     :n  "f" #'describe-function
      :desc "Describe face"         :n  "F" #'describe-face
      :desc "Info"                  :n  "i" #'info-lookup-symbol
      :desc "Describe key"          :n  "k" #'describe-key
      :desc "Find library"          :n  "l" #'find-library
      :desc "View *Messages*"       :n  "m" #'view-echo-area-messages
      :desc "Describe mode"         :n  "M" #'describe-mode
      :desc "Describe variable"     :n  "v" #'describe-variable
      )

      (:desc "Search" :prefix "s"
      :desc "Buffer"                 :nv "b" #'consult-line
      :desc "Project"                :nv "p" #'+project-rg
      :desc "Directory"              :nv "d" #'+vertico/project-search-from-cwd
      )

      (:desc "project" :prefix "p"
      :desc "Switch project"          :n  "p" #'project-switch-project
      :desc "Find file in project"    :n  "f" #'project-find-file
      :desc "Find dir in project"     :n  "F" #'project-find-dir
      :desc "Dired in project"        :n  "d" #'project-dired
      :desc "Find buffer in project"  :n  "b" #'consult-project-buffer
      :desc "Recent project files"    :n  "K" #'project-kill-buffers

      )

      (:desc "buffer" :prefix "b"
      :desc "New empty buffer"        :n "n" #'evil-buffer-new
      :desc "Switch buffer"           :n "b" #'switch-to-buffer
      :desc "Kill buffer"             :n "k" #'kill-this-buffer
      :desc "Kill other buffers"      :n "o" #'kill-other-buffers
      :desc "Save buffer"             :n "s" #'save-buffer
      :desc "Bury buffer"             :n "z" #'bury-buffer
      )

      (:desc "file" :prefix "f"
      :desc "Find file"                 :n "f" #'find-file
      :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
      :desc "Find directory"            :n "d" #'dired
      :desc "Rename buffer and file"    :n "R" #'sea/rename-this-file-and-buffer
      :desc "Delete this file"          :n "X" #'sea/delete-this-file)

      (:desc "git" :prefix "g"
      :desc "Magit blame"           :n  "b" #'magit-blame
      :desc "Magit commit"          :n  "c" #'magit-commit
      :desc "Magit dispatch"        :n  "d" #'magit-dispatch-popup
      :desc "Magit find-file"       :n  "f" #'magit-find-file
      :desc "Magit status"          :n  "g" #'magit-status
      :desc "Magit file delete"     :n  "x" #'magit-file-delete
      :desc "Initialize repo"       :n  "i" #'magit-init
      :desc "Magit buffer log"      :n  "l" #'magit-log-buffer-file
      :desc "List repositories"     :n  "L" #'magit-list-repositories
      :desc "Magit push popup"      :n  "p" #'magit-push-popup
      :desc "Magit pull popup"      :n  "P" #'magit-pull-popup
      :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
      :desc "Git revert file"       :n  "R" #'vc-revert
      :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
      :desc "Git stage file"        :n  "S" #'magit-stage-file
      :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
      :desc "Git unstage file"      :n  "U" #'magit-unstage-file
      :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
      :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)
)
