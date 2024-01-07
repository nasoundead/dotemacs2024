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
          :n "l" #'treemacs-RET-action)
        )

      ;; flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error

      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous
      
      ;; --- personal vim-esque bindings ------------------
      :n  "zx" #'kill-this-buffer
      :m  "]a" #'evil-forward-arg
      :m  "[a" #'evil-backward-arg
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :nv "gd" #'+lookup/definition
      :nv "gr" #'+lookup/references
      :nv "gi" #'+lookup/implementationss
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
          "s"       #'sea/split-window-horizontally-instead
          "v"       #'sea/split-window-vertically-instead
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