;; after cedet emacs23 buildin

(require 'cedet)
(require 'cedet-cscope)
(require 'semantic/db-ebrowse)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/c)
(require 'semantic-ia nil 'noerror)
(require 'semantic-c nil 'noerror)
(require 'semantic/db-global)
(require 'srecode)                      ;template

;; url: http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;; Helper tools.
(custom-set-variables
 '(semantic-idle-scheduler-idle-time 1) ; 空闲时进行补全分析,避免semantic占用CPU过多
 '(semantic-default-submodes
   '(global-semanticdb-minor-mode
     global-semantic-decoration-mode
     global-semantic-idle-completions-mode
     global-semantic-idle-scheduler-mode
     global-semantic-idle-summary-mode
     global-semantic-mru-bookmark-mode
     global-semantic-show-parser-state-mode
     ;; global-semantic-highlight-func-mode
     ;; global-semantic-highlight-edits-mode
     ;; global-semantic-show-unmatched-syntax-mode
     global-semantic-show-parser-state-mode
     global-semantic-stickyfunc-mode
     global-semantic-idle-local-symbol-highlight-mode))
 )
(semantic-mode 1)

;; smart complitions
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;; Include
(defconst cedet-user-include-dirs
  (list "." ".." "./inc" "../inc" "./include" "../include"))
(setq cedet-sys-include-dirs
      (list "/usr/include"
            "/usr/local/include"))
(setq semantic-c-dependency-system-include-path "/usr/include/")
;; save to var semantic-dependency-system-include-path
(let (include-dirs cedet-user-include-dirs)
  (setq include-dirs (append include-dirs cedet-sys-include-dirs))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

;; TAGS Menu
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)
(add-hook 'semantic-init-hooks 'semantic-idle-completions-mode) ;优先调用了senator的分析结果

;; Semantic db
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))
(setq semanticdb-search-system-databases t) ; C/C++语言启动时自动加载semantic对/usr/include的索引数据库
(setq semanticdb-project-roots              ; 设置semantic.cache路径
      (list
       (expand-file-name "/")))

;; gnu global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; keywords and stuff, set up indenting correctly for new kewords
(setq c-protection-key (concat "\\<\\(public\\|protected"
                               "\\|private"
                               "\\)\\>")
      c-C++-access-key (concat "\\<\\|public\\|protected\\|private"
                               "\\)\\>[ \t]*:"))
(setq c-macro-names-with-semicolon '("DEBUG"))
(c-make-macro-with-semi-re)

(autoload 'senator-try-expand-semantic "senator") ;配置Semantic的检索范围
;; hippie-expand-try
(setq hippie-expand-try-functions-list
      '(senator-try-expand-semantic
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        yas/hippie-try-expand
        semantic-ia-complete-symbol))

(defun my-indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)))

(defun my-cedet-hook ()
  "cedet hook key map"
  ;; Semantic functions.
  (semantic-default-c-setup)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cR" 'semantic-symref)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-ia-show-summary)
  (local-set-key "\C-cl" 'semantic-ia-show-doc)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key [(tab)] 'my-indent-or-complete))
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'c++-mode-common-hook 'my-cedet-hook)

(global-set-key [f12] 'semantic-ia-fast-jump)
(global-set-key [f5] ; return prev tag
                (lambda ()
                  (interactive)
                  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
                      (error "Semantic Bookmark ring is currently empty"))
                  (let* ((ring (oref semantic-mru-bookmark-ring ring))
                         (alist (semantic-mrub-ring-to-assoc-list ring))
                         (first (cdr (car alist))))
                    (if (semantic-equivalent-tag-p (oref first tag)
                                                   (semantic-current-tag))
                        (setq first (cdr (car (cdr alist)))))
                    (semantic-mrub-switch-tags first))))

;; cedet
(when (and window-system (require 'semantic-tag-folding nil 'noerror))
  (global-semantic-tag-folding-mode 1)
  (define-key semantic-tag-folding-mode-map (kbd "C-c , -") 'semantic-tag-folding-fold-block)
  (define-key semantic-tag-folding-mode-map (kbd "C-c , +") 'semantic-tag-folding-show-block)
  (define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-all)
  (define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-show-all)
  (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode))

;; semantic-mode
(semantic-mode t)

;; ede
;; Enable EDE (Project Management) features
(global-ede-mode t)
;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)
(setq srecode-map-load-path
      (list (srecode-map-base-template-dir)
            "~/.emacs.d/templates/srecode"))

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:
;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)
;; * This enables some tools useful for coding, such as summary mode
;; imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)
;; * This enables even more coding tools such as intellisense mode
;; decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)
;; * This enables the use of Exuberent ctags if you have it installed.
;; If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;; Or, use one of these two types of support.
;; Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;; Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

(provide 'init-cedet)
