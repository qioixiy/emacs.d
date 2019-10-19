;;; auto-complete

(require 'auto-complete-config)

;; auto-complete
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict")
(ac-set-trigger-key "TAB")
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
(setq ac-comphist-file "~/.emacs.d/site-lisp/data/auto-complete/ac-comphist.dat")
(setq ac-use-quick-help nil)
(setq ac-auto-start 3) ;; autocomplete start from 3 char
(setq ac-auto-show-menu 0.5);; Show menu 0.5 second later
(setq ac-menu-height 20);; menu height 20 lines
(setq ac-use-menu-map t)
(setq-default
 ac-sources '(ac-source-yasnippet
			  ac-source-semantic
			  ac-source-ropemacs
			  ac-source-abbrev
			  ac-source-dictionary
			  ac-source-filename
			  ac-source-imenu
			  ac-source-words-in-buffer
			  ac-source-files-in-current-dir
			  ac-source-words-in-same-mode-buffers))

(provide 'init-auto-complete)
