
;;;
;;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org

;; setq-default
(setq-default
 compile-command "make"
 cursor-type 'bar               ;设置光标为线条状
 ispell-program-name "aspell"	;是用aspell程序作为Emacs的拼写检查成学
 indent-tabs-mode nil
 kill-whole-line t                      ;使用C-k删掉指针到改行末的所有东西
 make-backup-files nil			;不要生成临时文件
 c-default-style "linux"
 c-basic-offset 4
 )

;; setq
(setq
 auto-fill-mode t
 appt-issue-message t                ;约会提醒
 apropos-do-all t                    ;增大使用查找函数和变量的寻找范围
 backup-directory-alist '(("." . "~/.emacs.d/.backup")) ;设置备份文件的路径
 backup-by-copying t                    ;备份设置方法，直接拷贝
 backup-enable-predicate 'my-ecm-backup-enable-predicate ;设置备份条件
 c-macro-cppflags " "
 c-macro-shrink-window-flag t           ;预处理设置
 c-macro-preprocessor "cpp"             ;c/cpp
 c-macro-prompt-flag t
 column-number-mode t                ;show column number on status bar
 delete-old-versions t               ;删掉不属于以上3种版本的版本
 default-buffer-file-coding-system 'utf-8 ;新建文件的编码方式
 default-major-mode 'text-mode            ;打开就启用 text 模式
 default-tab-width 4                      ;设置tab为4个空格的宽度
 dired-recursive-copies 'top         ;让dired 可以递归的拷贝和删除目录
 dired-recursive-deletes 'top
 dired-kept-versions 1
 enable-recursive-minibuffers t  ;可以递归的使用 minibuffer
 fill-column 120                 ;Set line width to 120 columns
 frame-title-format "%b@emacs"   ;在标题栏显示buffer的名字(默认不显示)
 file-name-coding-system 'utf-8  ;读取或写入文件名的编码方式
 inhibit-startup-message t       ;禁用启动信息
 kill-ring-max 200        ;设定删除保存记录为200，可以方便以后无限恢复
 kept-old-versions 2 ;备份最原始的版本两次，即第一次编辑前的文档，和第二次编
 kept-new-versions 6 ;备份最新的版本6次，理解同上
 mouse-yank-at-point t                  ;不要在鼠标光标出插入
 require-final-newline t ;在文档最后自动插入空白一行，好像某些系统配置文件是需要这样的,自动的在文件末增加一新行
 scroll-margin 3         ;3行时就开始滚动，可以很好的看到上下文
 scroll-conservatively 10000
 scroll-step 1                     ;滚动页面时比较舒服，不要整页的滚动
 shell-file-name "/bin/bash"
 show-paren-style 'expression           ; parentheses,expression
 track-eol t             ;当光标在行尾上下移动的时候，始终保持在行尾。
 tab-stop-list ()
 user-full-name "qioixiy"
 user-mail-address "*@*"
 vc-make-backup-files t            ;使用版本控制系统的时候也启用备份功能
 version-control t                 ;启用版本控制，即可以备份多次
 visiable-bell t                   ;禁止终端响铃
 x-select-enable-clipboard t       ;支持emacs和外部程序的粘贴
 ;;----------------------------------------------------------------------------
 ;; Which functionality to enable (use t or nil for true and false)
 ;; https://github.com/redguardtoo/emacs.d/blob/master/init.el
 ;;----------------------------------------------------------------------------
 *macbook-pro-support-enabled* t
 *is-a-mac* (eq system-type 'darwin)
 *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac))
 *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns))
 *win32* (eq system-type 'windows-nt)
 *cygwin* (eq system-type 'cygwin)
 *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux))
 *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix))
 *linux-x* (and window-system *linux*)
 *xemacs* (featurep 'xemacs)
 *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24)))
 *no-memory* (cond
              (*is-a-mac*
               (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
              (*linux* nil)
              (t nil))
 font-encoding-alist
 (append '(("MuleTibetan-0" (tibetan . 0))
           ("GB2312" (chinese-gb2312 . 0))
           ("JISX0208" (japanese-jisx0208 . 0))
           ("JISX0212" (japanese-jisx0212 . 0))
           ("VISCII" (vietnamese-viscii-lower . 0))
           ("KSC5601" (korean-ksc5601 . 0))
           ("MuleArabic-0" (arabic-digit . 0))
           ("MuleArabic-1" (arabic-1-column . 0))
           ("MuleArabic-2" (arabic-2-column . 0))) font-encoding-alist)
 )

(defun my-ecm-backup-enable-predicate (filename) ;关闭匹配下列目录或文件的备份功能
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "semanticdb" filename))))

;; auto pair
(defun my-common-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist '(
                              (?` ?` _ "''")
                              (?\( _ ")")
                              (?\[ _ "]")
                              (?{ \n > _ \n ?} >)
                              (?\" _ "\"")))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe))

(defun my-lang-style-setting()
  (interactive)
  (c-set-style "K&R")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (local-set-key (kbd "RET") 'newline-and-indent))

;; 匹配的括号之间来回跳转, C-M-f,C-M-b,%
(defun my-match-paren-1 (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun my-match-paren-2 (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

(defun my-goto-match (arg)
  (interactive "p")
  (let ((stop nil) (c 1) (p-save (point)) (forward t) (self "") (target ""))
    (cond ((looking-at "}") (setq forward nil) (setq self "}") (setq target "{"))
          ((looking-at "{") (setq forward t)   (setq self "{") (setq target "}"))
          ((looking-at ")") (setq forward nil) (setq self ")") (setq target "("))
          ((looking-at "(") (setq forward t)   (setq self "(") (setq target ")"))
          ((looking-at ">") (setq forward nil) (setq self ">") (setq target "<"))
          ((looking-at "<") (setq forward t)   (setq self "<") (setq target ">"))
          ((looking-at "\\]") (setq forward nil) (setq self "\\]") (setq target "\\["))
          ((looking-at "\\[") (setq forward t)   (setq self "\\[") (setq target "\\]"))
          (t (setq stop t) (setq c -1)))
    (while (not stop)
      (progn
        (if forward (forward-char 1) (backward-char 1))
        (cond ((looking-at target) (setq c (1- c)))
              ((looking-at self) (setq c (1+ c))))
        (if (or (= c 0) (= (point) (point-max))) (setq stop t))))
    (if (= c -1) (self-insert-command (or arg 1)))
    (if (> c 0) (goto-char p-save))))

(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)))

;; 有时你需要跳到另一个文件进行一些操作，然后很快的跳回来。你当然可以使用bookmark或者寄存器。
;; push and pop a tmp postion
(defun my-ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun my-ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

;; my-go-to-char
(defun my-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `my-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(defun rename-buffer-in-ssh-login (cmd)
  "Rename buffer to the destination hostname in ssh login"
  (if (string-match "ssh [-_a-z0-9A-Z]+@[-_a-z0-9A-Z.]+[ ]*[^-_a-z0-9-A-Z]*$" cmd)
      (let (( host (nth 2 (split-string cmd "[ @\n]" t) )))
        (rename-buffer (concat "*" host)) ;
        (add-to-list 'shell-buffer-name-list (concat "*" host));
        (message "%s" shell-buffer-name-list))))
(defun kill-shell-buffer(process event)
  "The one actually kill shell buffer when exit. "
  (kill-buffer (process-buffer process)))

(defun kill-shell-buffer-after-exit()
  "kill shell buffer when exit."
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-shell-buffer))
(add-hook 'comint-input-filter-functions 'rename-buffer-in-ssh-login)
(add-hook 'shell-mode-hook 'kill-shell-buffer-after-exit t)
(defun my-recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl)))))

(defun my-current-line-to-top-of-window ()
  "Move the line point is on to top of window."
  (interactive)
  (recenter 0))

;; etags generate-tag-table
(defun my-generate-tag-table()
  "Generate tag tables under current directory(Linux)."
  (interactive)
  (let
      ((exp "")
       (dir ""))
    (setq dir (read-from-minibuffer "generate tags in:" default-directory)
          exp (read-from-minibuffer "suffix:" "*.[ch]"))
    (setq command_local (concat "rm -f TAGS && find " dir " -name " "\"" exp "\"" " | xargs -n 1 etags -a"))
    (message command_local)
    (message "tags generating...")
    (with-temp-buffer
      (shell-command
       command_local
       (buffer-name)))
    (message "tags generated!")))

(defun my-save-code-to-library()
  (interactive)
  (let (
        (mode-file-map '((c++-mode . "cpp.org")
                         (emacs-lisp-mode . "elisp.org")
                         (python-mode . "python.org")
                         (perl-mode . "perl.org")
                         (dos-mode . "bat.org")
                         (sh-mode . "bash.org")))
        (code-library-path "~/.emacs.d/code-library/")
        (code (get-buffer-window-list))
        (library-file (concat code-library-path (cdr (assoc major-mode mode-file-map))))
        (head (read-string "please input desc:"))
        (code-major-mode (replace-regexp-in-string "-mode$" "" (format "%s" major-mode))))
    (when (string= library-file code-library-path)
      (setq library-file (concat code-library-path "temp.org")))
    (find-file library-file)
    (end-of-buffer)
    (newline)
    (insert (concat "* " head))
    (newline-and-indent)
    (insert (concat "#+BEGIN_SRC " code-major-mode))
    (newline-and-indent)
    (newline-and-indent)
    (insert "#+END_SRC")
    (forward-line -1)
    (org-edit-src-code)
    (insert code)
    (org-edit-src-exit)
    (org-set-tags-command)
    (save-buffer)
    (kill-buffer)
    ))

(defun my-semantic-database ()
  (setq semanticdb-project-system-databases
        (list (semanticdb-create-database
               semanticdb-new-database-class
               "/usr/include"))))

;; tab->space
;; (untabify)
;; space->tab
;; (tabify)

(require 'cc-mode)
(require 'ibuffer);; buffer menu
(require 'browse-kill-ring);; browse-kill-ring
(require 'expand-region)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; 代码块收缩
(require 'hideshow)
(setq hs-minor-mode t)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(add-hook 'c-mode-hook 'my-lang-style-setting)
(add-hook 'c++-mode-hook 'my-lang-style-setting)

(add-hook 'c-mode-hook 'my-common-mode-auto-pair)
(add-hook 'c++-mode-hook 'my-common-mode-auto-pair)
(add-hook 'java-mode-hook 'my-common-mode-auto-pair)
(add-hook 'lisp-mode-hook 'my-common-mode-auto-pair)
(add-hook 'php-mode-hook 'my-common-mode-auto-pair)
(add-hook 'python-mode-hook 'my-common-mode-auto-pair)
(add-hook 'html-mode-hook 'my-common-mode-auto-pair)
(add-hook 'scheme-mode-hook 'my-common-mode-auto-pair)
(add-hook 'css-mode-hook 'my-common-mode-auto-pair)
(add-hook 'sql-mode-hook 'my-common-mode-auto-pair)
(add-hook 'emacs-lisp-mode-hook 'my-common-mode-auto-pair)
(add-hook 'text-mode-hook 'my-common-mode-auto-pair)
(add-hook 'cperl-mode-hook 'my-mode-auto-pair)
(add-hook 'perl-mode-hook 'my-mode-auto-pair)

;;鼠标滚轮，默认的滚动太快，这里改为1行
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(global-set-key [(control tab)] 'my-indent-or-complete)

;; nav
(require-package 'nav)
(require 'nav)
(nav-disable-overeager-window-splitting)
;; Optional: set up a quick key to toggle nav
(global-set-key [f1] 'nav-toggle)

;; hippie-expand diff with dabbrev-expand
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(senator-try-expand-semantic        ;优先调用senator的分析结果
        try-expand-dabbrev-visible         ;dabbrev策略, 可见窗口优先
        try-expand-dabbrev                 ;dabbrev策略
        try-expand-dabbrev-all-buffers     ;dabbrev策略, 包括所有窗口(除了当前窗口)
        try-expand-dabbrev-from-kill       ;dabbrev策略, 从所有删除记录里搜索
        try-complete-file-name             ;补全文件名
        try-complete-file-name-partially   ;补全文件名, 匹配优先
        try-expand-list                    ;补全list
        try-expand-list-all-buffers        ;补全list, 包括所有窗口(除了当前窗口)
        try-expand-all-abbrevs
        try-expand-line                    ;整行补全
        try-expand-line-all-buffers        ;整行补全, 包括所有窗口(除了当前窗口)
        try-complete-lisp-symbol           ;补全符号, 符号太多了, 设置低优先级利于高效补全
        try-complete-lisp-symbol-partially ;补全符号, 包括所有窗口(除了当前窗口)
        try-expand-whole-kill              ;kill-ring里面补全
        ))

;; misc
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode t)

;; 显示时间，格式如下
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; 关闭自动保存模式
(setq auto-save-mode nil)
(setq auto-save-default nil)            ;不生成 #filename# 临时文件

;; windows os spec
(if (eq system-type `windows-nt)
    (let ()
      (run-with-idle-timer 0.1 nil 'w32-send-sys-command 61488)
      (custom-set-variables
       '(custom-enabled-themes (quote (dichromacy)))
       '(inhibit-startup-screen t)
       '(tool-bar-mode nil))))

;; UTF-8
(if 0
    (lambda()
      (setq current-language-environment "UTF-8")
      (setq default-input-method "chinese-py")
      (setq locale-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)
      (prefer-coding-system 'utf-8)
      ;; get around the Ctrl+SPACE key binding for inputing method
      (global-set-key "\C-cm" 'set-mark-command)))

;;(require-package 'hide-region)
(require-package 'hide-lines)
;;(require 'hide-region)
(require 'hide-lines)
(define-key global-map [(meta f11)] 'hs-hide-block) ;;gui f11 global map
(define-key global-map [(meta f12)] 'hs-show-block)

(global-set-key "%" 'my-match-paren-2)
(global-set-key "\M-/" 'auto-complete);; force start auto complete
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key (kbd "C-x 4 c") (quote change-split-type-3))
(global-set-key (kbd "C-x 4 r")  (quote roll-v-3))

;; Package: clean-aindent-mode
(maybe-require-package 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
;; Package: dtrt-indent
(maybe-require-package 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)
;; Package: ws-butler
(maybe-require-package 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)

;; auto formated when yank-pop
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(
                     c-mode
                     c++-mode
                     clojure-mode
                     emacs-lisp-mode
                     haskell-mode
                     js-mode
                     latex-mode
                     lisp-mode
                     objc-mode
                     perl-mode
                     cperl-mode
                     plain-tex-mode
                     python-mode
                     rspec-mode
                     ruby-mode
                     scheme-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; 存盘前删除行末多余的空格/空行
;;(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;; ;; create new prefix command, ctrl-z
;; (define-prefix-command 'ctl-z-map)
;; (global-set-key (kbd "C-z") 'ctl-z-map)
;; (global-set-key (kbd "C-z C-z") 'suspend-frame)
;; (global-set-key (kbd "C-z C-x") 'kill-emacs)

;; ;; global-set-key
;; (global-set-key (kbd "C-c z") 'shell)
;; (global-set-key (kbd "<f3>") 'rename-buffer)

(when (version< emacs-version "26.0.50")
  (global-linum-mode))

;; common mode
(auto-image-file-mode t)                ;打开图片显示功能
(auto-save-mode 0)                      ;取消自动保存
(browse-kill-ring-default-keybindings)
(blink-cursor-mode t)                   ;设置光标不闪
(column-number-mode t)                  ;设置是否显示光标所在列号
(display-time-mode t)                   ;显示时间
(global-font-lock-mode t)               ;语法高亮
(ido-mode t)
(menu-bar-mode nil)                     ;去掉菜单栏
(mouse-avoidance-mode 'animate)         ;光标靠近鼠标指针时，让鼠标指针自动让开
(put 'set-goal-column 'disabled nil)    ;把这些缺省禁用的功能打开
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)   ;使用narrow功能时的一个设置
(put 'LaTeX-hide-environment 'disabled nil)
;; (set-default-font "10x20")       ;Font,"10x20" "STHeiti-10" "Droid Sans Mono-10"
;; (set-language-environment 'Chinese-GB)  ;设置中文语言环境
(set-buffer-file-coding-system 'utf-8)  ;写文件的编码方式,gb2312,utf-8
(set-terminal-coding-system 'utf-8)     ;终端方式的编码方式
(set-keyboard-coding-system 'utf-8)     ;键盘输入的编码方式
;; (set-background-color "white")          ;设置背景颜色
;; (set-foreground-color "white")          ;设置字体颜色
;; (set-scroll-bar-mode 'right)            ;滚动条在右侧
;; (scroll-bar-mode nil)   ;隐藏滚动条。实际上emacs-nox是没有这个模式的。
(show-paren-mode t)     ;显示括号匹配
(tool-bar-mode nil)     ;隐藏工具栏
(transient-mark-mode t) ;高亮显示选中的区域
(which-function-mode)   ;在模式栏中显示当前光标所在函数
;; (whitespace-mode)
(global-hl-line-mode t)                 ;高亮当前行
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x 4) tab-stop-list)))
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

;; time-stamp,只要里在你得文档里有Time-stamp:的设置，就会自动保存时间戳
(setq time-stamp-active t)              ;启用time-stamp
(setq time-stamp-warn-inactive t)       ;去掉time-stamp的警告
(setq time-stamp-format "%:u %02m/%02d/%04y %02H02M02S")
(add-hook 'before-save-hook 'time-stamp)

;;将文件模式和文件后缀关联起来
(mapcar
 (function (lambda (setting)
             (setq auto-mode-alist
                   (cons setting auto-mode-alist))))
 '(("\\.xml" .  sgml-mode)
   ("\\.bash" . sh-mode)
   ("\\.rd" .  sgml-mode)
   ("\\.session" . emacs-lisp-mode)
   ("\\.c" . c-mode)
   ("\\.h" . c-mode)
   ("\\.hpp" . c++-mode)
   ("\\.cc" . c++-mode)
   ("\\.cpp" . c++-mode)
   ("\\.cxx" . c++-mode)
   ("\\.css" . css-mode)
   ("\\.html" . html-mode)
   ("\\.s?html?\\'" . html-helper-mode)
   ("\\.asp\\'" . html-helper-mode)
   ("\\.phtml\\'" . html-helper-mode)
   ("\\.css\\'" . css-mode)
   ("\\.pc" . c-mode)
   ("\\.py\\'" . python-mode)
   ("gnus" . emacs-lisp-mode)
   ("\\.idl" . idl-mode)))

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq-default auto-fill-function 'do-auto-fill)

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 4)				   ; 默认风格是4个缩进
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))				; 默认风格是4个缩进
(add-hook 'c-run-mode-hooks				;c-mode或cc-mode下缩进只有4格
          '(lambda ()
             (c-set-style "Stroustrup")))

;; my-cpp-stype
(defconst my-cpp-style
  '((c-tab-always-indent . t)
	;; (c-comment-only-line-offset . 0)
	(c-hanging-braces-alist . ((substatement-open after)
							   (brace-list-open)))
	(c-cleanup-list . (comment-close-slash
					   compact-empty-funcall))
	(c-offsets-alist . ((substatement-open . 0)
						(innamespace . 0) ;;在namespace中不缩进
						(case-label . +) ;;case标签缩进一个c-basic-offset单位
						(access-label . -) ;;private/public等标签少缩进一单位
						(inline-open . 0) ;;在.h文件中写函数，括号不缩进
						(block-open . 0))) ;;在一个新块开始时不缩进
	;;(c-echo-syntactic-information-p t)
	(setq comment-start "/*"
		  comment-end "*/"
		  indent-tabs-mode nil)
	)
  "my cpp coding style")

(c-add-style "my-cpp-style" my-cpp-style) ; 定义自己的style，将其添加到cc-mode的style中。

(setq auto-mode-alist
      (cons '("/usr/src/linux.*/.*\\.[ch]" . linux-c-mode)
            auto-mode-alist))

(c-add-style "my-c/cpp-style"
             '("gnu"
               (fill-column . 120)
               (c++-indent-level . 4)
               (c-basic-offset . 4)
               ;; (comment-start . "/*")
               ;; (comment-end . "*/")
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset . 0)
               (c-tab-always-indent . t)
               (c-hanging-braces-alist . ((substatement-open after)
			                  (brace-list-open)))
               (c-cleanup-list . (comment-close-slash
		                  compact-empty-funcall))
               (c-offsets-alist . ((arglist-intro . +)
                                   (innamespace . 0)
                                   (member-init-intro . +)
                                   ;; 在一个新块开始时不缩进
                                   (substatement-open . 0)
                                   ;; case标签缩进一个c-basic-offset单位
                                   (case-label . +)
                                   ;; private/public等标签少缩进一单位
			           (access-label . -)
                                   ;;在.h文件中写函数，括号不缩进
			           (inline-open . 0)
			           (block-open . 0)))))

;; c/c++ coding style.
(add-hook 'c-mode-common-hook
          (function
           (lambda nil
             (progn
               (c-set-style "my-c/cpp-style")
               (local-set-key  (kbd "C-c o") 'ff-find-other-file)
               (my-semantic-database)
               (message "use c common style")
               ))))

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)

;;; shell
;;; C-u output to current buffer
;;; shell-command(M-!)/async-shell-command(M-&)/shell-command-on-region(M-|)

;;; C-u M-! date
(defun my-insert-time-string ()
  "Insert the date in current position."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M:%S]")))

;;; 
(defun my-GDT (filename &optional wildcards)
  "base on find-file"
  (interactive (cons (concat
                      (read-file-name "Find file: "
                                      (concat user-emacs-directory "GDT/") nil))
                     (cons t nil)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))))

;;; https://stackoverflow.com/questions/3216081/integrate-emacs-copy-paste-with-system-copy-paste
;; CUA OS copypasta even in ncurses mode
(case system-type
  ('darwin (unless window-system
             (setq interprogram-cut-function
                   (lambda (text &optional push)
                     (let* ((process-connection-type nil)
                            (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
                       (process-send-string pbproxy text)
                       (process-send-eof pbproxy))))))
  ('gnu/linux (progn
                ;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
                ;; I prefer using the "clipboard" selection (the one the
                ;; typically is used by c-c/c-v) before the primary selection
                ;; (that uses mouse-select/middle-button-click)
                (setq x-select-enable-clipboard t)

                ;; If emacs is run in a terminal, the clipboard- functions have no
                ;; effect. Instead, we use of xsel, see
                ;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
                ;; program for getting and setting the contents of the X selection"
                (unless window-system
                  (when (getenv "DISPLAY")
                    ;; Callback for when user cuts
                    (defun xsel-cut-function (text &optional push)
                      ;; Insert text to temp-buffer, and "send" content to xsel stdin
                      (with-temp-buffer
                        (insert text)
                        ;; I prefer using the "clipboard" selection (the one the
                        ;; typically is used by c-c/c-v) before the primary selection
                        ;; (that uses mouse-select/middle-button-click)
                        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
                    ;; Call back for when user pastes
                    (defun xsel-paste-function()
                      ;; Find out what is current selection by xsel. If it is different
                      ;; from the top of the kill-ring (car kill-ring), then return
                      ;; it. Else, nil is returned, so whatever is in the top of the
                      ;; kill-ring will be used.
                      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
                        (unless (string= (car kill-ring) xsel-output)
                          xsel-output )))
                    ;; Attach callbacks to hooks
                    (setq interprogram-cut-function 'xsel-cut-function)
                    (setq interprogram-paste-function 'xsel-paste-function)
                    ;; Idea from
                    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
                    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
                    ))))
  ('gnu/linux (progn
                (setq x-select-enable-clipboard t)
                (defun xsel-cut-function (text &optional push)
                  (with-temp-buffer
                    (insert text)
                    (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
                (defun xsel-paste-function()

                  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
                    (unless (string= (car kill-ring) xsel-output)
                      xsel-output )))
                (setq interprogram-cut-function 'xsel-cut-function)
                (setq interprogram-paste-function
                      'xsel-paste-function))))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis
                      after-init-time before-init-time))))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (setq semanticdb-project-system-databases
;;                   (list (semanticdb-create-database
;;                          semanticdb-new-database-class
;;                          "/usr/include")))))

;;; ggtags mode
(require 'init-ggtags)

(provide 'init-private)

;;; init-private ends here
