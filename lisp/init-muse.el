;;; http://alexott.net/en/writings/EmacsMuseMyPage.html
(require 'muse-mode)
(require 'muse-html)
(require 'muse-colors)
(require 'muse-wiki)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-project)

(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))

(custom-set-variables
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-html-charset-default "utf-8")
 '(muse-file-extension "muse")
 '(muse-mode-auto-p nil)
 '(muse-wiki-allow-nonexistent-wikiword nil)
 '(muse-wiki-use-wikiword nil)
 '(muse-ignored-extensions (quote ("bz2" "gz" "[Zz]" "rej" "orig" "png" "hgignore" "gif"
                                   "css" "jpg" "html" "sh" "lftp" "pdf")))
 )

(defun my-muse-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (footnote-mode 1))
(add-hook 'muse-mode-hook 'my-muse-mode-hook)

;;; templete
(muse-derive-style "my-page-html" "html"
                   :header "~/projects/my-page-muse/header.tmpl"
                   :footer "~/projects/my-page-muse/footer.tmpl")

(muse-derive-style "my-page-pdf" "pdf"
                   :header "~/projects/my-page-muse/header.tex"
                   :footer "~/projects/my-page-muse/footer.tex")

(setq my-muse-projects-dir "projects/my-page-muse")

;;; site
(setq muse-project-alist
      `(
        ("my-page"
         (,@(muse-project-alist-dirs "~/projects/my-page-muse") :default "index")
         ,@(muse-project-alist-styles "~/projects/my-page-muse"
                                      "~/projects/my-page-muse"
                                      "my-page-html")
         (:base "my-page-pdf"
                :path "~/projects/my-page-muse/en"
                :include "/alexott-cv-en[^/]*$")
         (:base "my-page-pdf"
                :path "~/projects/my-page-muse/ru"
                :include "/alexott-cv-ru[^/]*$"))))

(defun muse-gen-relative-name (name)
  (concat
   (file-name-directory (muse-wiki-resolve-project-page (muse-project)))
   name))

(defun muse-mp-detect-language ()
  (let ((lang "NN")
        (cur-dir (file-name-directory (muse-current-file)))
        )
    (let ((smatch (string-match "/\\(ru\\|en\\|de\\)/" cur-dir)))
      (when smatch
        (setq lang (substring cur-dir (+ smatch 1) (+ smatch 3)))))
    lang))

(setq my-page-menu '(("ru" . ( ("index.html" . "Главная")
                               ("cf/" . "Информационная безопасность")
                               ("fp/" . "Функциональное программирование")
                               ("scheme/" . "Scheme")
                               ("lisp/" . "emacs-lisp")
                               ("cpp/" . "C++")
                               ("oss/" . "Open Source Projects")
                               ("emacs/" . "Emacs")
                               ("writings/" . "Статьи")))
                     ("en" . ( ("index.html" . "Main")
                               ("cf/" . "Information Security")
                               ("fp/" . "Functional programming")
                               ("cpp/" . "C++")
                               ("oss/" . "Open Source Projects")
                               ("emacs/" . "Emacs")
                               ("writings/" . "Articles")))))

(defun muse-mp-generate-menu ()
  (let* ((menu-lang (muse-mp-detect-language))
         (menu-struct (assoc menu-lang my-page-menu))
         (menu-string "")
         (rel-dir (file-name-directory (muse-wiki-resolve-project-page (muse-project))))
         (rel-path (if (> (length rel-dir) 2)   (substring rel-dir 3) ""))
         (cur-path-muse (muse-current-file))
         (cur-path-html (replace-regexp-in-string "\\.muse" ".html" cur-path-muse))
         )
    (when menu-struct
      (let ((menu-list (if (not (null menu-struct)) (cdr menu-struct))))
        (setq menu-string
              (concat "<ul class=\"avmenu\">"
                      (apply #'concat
                             (mapcar
                              (lambda (x)
                                (concat "<li><a href=\"" rel-path (car x)
                                        (if (string-match (concat "/" menu-lang "/" (car x))
                                                          cur-path-html)
                                            "\" class=\"current\""
                                          "\"")
                                        ">" (cdr x) "</a></li>"))
                              menu-list))
                      "</ul>"))))
    menu-string))

(defun generate-change-date (file)
  (when (file-exists-p file)
    (let* ((fa (file-attributes file))
           (mod-time (nth 6 fa)))
      (format-time-string "%d.%m.%Y %R" mod-time))))
