(require-package 'dictionary)
(require 'dictionary)

;; install dictd
;;sudo apt-get install dictd dict dict-stardic
;;sudo service dictd start
;;dict -D ; test dict server

;; choose a dictionary server
;; for dictionary tooltip mode
;; choose the dictionary: "wn" for WordNet
;; "web1913" for Webster's Revised Unabridged Dictionary(1913) so on
;; choose a dictionary server
;; (setq dictionary-server "wn")
(setq dictionary-server "localhost")

;; Tool-tip support for GNU Emacs 21
;; (setq dictionary-tooltip-dictionary "eng-deu")
;; (setq dictionary-tooltip-dictionary "wn")
(setq dictionary-tooltip-dictionary "localhost")
(global-dictionary-tooltip-mode t)
;;(dictionary-tooltip-mode t)

;; key bindings
(global-set-key "\C-cd" 'dictionary-lookup-definition)
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)

;; Popup menu for GNU Emacs 21, and XEmacs
(if (boundp 'running-xemacs)
	(global-set-key [(control button3)] 'dictionary-mouse-popup-matching-words)
  (global-set-key [mouse-3] 'dictionary-mouse-popup-matching-words))

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)

(provide 'init-dictionary)
