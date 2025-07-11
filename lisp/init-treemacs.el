;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:
;; A tree layout file explorer
(use-package treemacs
  :defines winum-keymap
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (
	 ([f8]        . treemacs)
	 ("M-0"       . treemacs-select-window)
	 :map treemacs-mode-map
	 ([mouse-1]   . treemacs-single-click-expand-action))

  :config
  (setq
   treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
   treemacs-width                           30
   treemacs-missing-project-action          'remove
   treemacs-follow-after-init               t
   treemacs-text-scale                      nil
   treemacs-user-mode-line-format           nil
   treemacs-user-header-line-format         nil
   treemacs-select-when-already-in-treemacs 'move-back
   treemacs-space-between-root-nodes        t
   treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
   treemacs-project-follow-into-home        nil
   treemacs-show-cursor                     nil
   treemacs-show-hidden-files               t
   treemacs-silent-filewatch                nil
   treemacs-silent-refresh                  nil
   treemacs-sorting                         'alphabetic-asc
   treemacs-hide-dot-git-directory          t
   treemacs-indentation                     2
   ;;  treemacs-indentation-string              " "
   treemacs-indent-guide-style              'block
   )

  (treemacs-follow-mode t)
  ;; (treemacs-load-all-the-icons-with-workaround-font)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)
  (pcase (cons (not (null (executable-find "git")))
	       (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (treemacs-hide-gitignored-files-mode nil)
  )

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
	  git-commit-post-finish
	  magit-post-stage
	  magit-post-unstage)
	 . treemacs-magit--schedule-update))

(use-package treemacs-nerd-icons
  :after treemacs
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config 
  (treemacs-load-theme "nerd-icons"))

;; (use-package treemacs-all-the-icons)

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
