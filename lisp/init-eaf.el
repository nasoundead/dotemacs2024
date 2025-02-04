(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/pdf-viewer")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/browser")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/netease-cloud-music")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/git")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/app/jupyter")
(require 'eaf)
(require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-git)
;; (require 'eaf-netease-cloud-music)
(require 'eaf-jupyter)


(use-package eaf
 :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
 :custom
 (eaf-browser-continue-where-left-off t)
 (eaf-browser-enable-adblocker t)
 (browse-url-browser-function 'eaf-open-browser)
 :config
 (defalias 'browse-web #'eaf-open-browser)
 (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
 (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
 (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki


(provide 'init-eaf)
