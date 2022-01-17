;;; package --- Personal org setup
;;; Commentary:
;; Copyright (C) 2015 Ravindra R. Jaju
;; Author: Ravindra Jaju - https:/msync.org/

;;; Code:
;;;

(require 'org)

;; Setup configuration
(setq org-directory "~/.org/" ;; Places where the agenda files exist.
      org-agenda-files '("~/.org/agenda")
      org-log-done t ;; This sets timestamps on tasks when finished.
      org-startup-indented t
      org-src-fontify-natively t
      org-default-notes-file "~/.org/on-the-fly-notes.org"
      org-fontify-whole-heading-line t
      org-return-follows-link t
      org-hide-emphasis-markers nil
      org-image-actual-width nil
      org-src-preserve-indentation t)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "DELEGATED")  ;; Tasks
        (sequence "SCHEDULE" "|" "MEETING-OVER")            ;; Meetings
        (sequence "RAW" "REFINE" "|" "IGNORE" "RECORDED"))) ;; Ideas


;; Org-babel

(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)
(add-to-list 'org-src-lang-modes '("clojure" . clojure))
;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP interactions
;;;;;;;;;;;;;;;;;;;;;;;
(use-package ob-http)
(use-package restclient)
(use-package ob-restclient)

;; Diagrams
(use-package plantuml-mode)

;;;;;;;;;;;;;;;;;;;;
;; Moar languages
;;;;;;;;;;;;;;;;;;;;
(require 'ob-js)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (clojure . t)
   (ditaa . t)
   (js . t)
   (java . t)
   (shell . t)
   (makefile . t)
   (org . t)
   (sql . t)
   (plantuml . t)
   (http . t)
   (restclient . t)
   (rust . t)))
(setq org-confirm-babel-evaluate nil)

;; More from http://fgiasson.com/blog/index.php/2016/04/05/using-clojure-in-org-mode-and-implementing-asynchronous-processing/
;; (org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
;; (org-defkey org-mode-map "\C-c\C-d" 'cider-doc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publishing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ox-hugo
  :ensure t
  :after (ox))
(use-package ox-reveal
  :ensure t
  :after (ox))

(require 'org-element)
(require 'ox-publish)
(with-eval-after-load 'ox
  (require 'ox-hugo)
  (require 'ox-html)
  (require 'ox-reveal))

(setq org-html-html5-fancy t
      org-src-tab-acts-natively t
      org-reveal-root "https://p.msync.org/reveal.js")

;; more reveal.js setup
(setq org-reveal-external-plugins
      '((animate . "{src: '%splugin/animate/animate.js', async: true, condition: function() { return !!document.body.classList; }}")
	(anything . "{src: '%splugin/animate/anything.js', async: true, condition: function() { return true; }}")))


(define-skeleton org-post-skeleton
  "Inserts the right directives for hugo-orgmode blogging."
  "Title: "
  "** " str "\n"
  ":PROPERTIES:\n"
  ":EXPORT_FILE_NAME: " (replace-regexp-in-string " " "-" (downcase str)) "\n"
  ":EXPORT_DATE: " (ut/date) "\n"
  ":EXPORT_HUGO_MENU: :menu \"main\"\n"
  ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :key value\n"
  ":END:\n")

;; From https://lucidmanager.org/productivity/create-websites-with-org-mode-and-hugo/
(setq time-stamp-active t
        time-stamp-start "#\\+LASTMOD:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "%Y-%02m-%02dT%02H:%02M:%02S%5z")
(add-hook 'before-save-hook 'time-stamp nil)

(define-skeleton org-note-skeleton
  "Inserts the right directives for notes."
  "Title: "
  "#+TITLE: " str "\n"
  "#+SUMMARY: \n"
  "#+DATE: " (ut/now) "\n"
  "#+LASTMOD: " (ut/now) "\n"
  "#+TAGS[]: \n"
  "#+CATEGORIES[]: \n"
  "#+DRAFT: true\n"
  "#+PROPERTY: header-args:clojure :exports source :results output :comments link :session *clojure-nrepl*\n"
  "#+PROPERTY: header-args:python :exports source :results output :comments link :session *python-dl*\n"
  "#+PROPERTY: header-args:bash :exports source :results output :comments link :session *shell*\n")

(setq org-publish-project-alist
      '(
        ("msync-notes"
         :base-directory "~/.org/msync/notes/"
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-attachment
         :publishing-directory "~/Projects/hugo-blog/content/notes"
	 :with-date t
	 )
	
        ("msync-notes-static"
         :base-directory "~/.org/msync/notes"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|py\\|clj\\|cljs\\|json\\|txt"
         :recursive t
         :publishing-directory "~/Projects/hugo-blog/content/notes"
         :publishing-function org-publish-attachment)
	
        ("msync"
         :components ("msync-notes" "msync-notes-static"))
	
        ("msync-presentation"
         :base-directory "~/.org/presentations"
         :publishing-directory "~/Projects/hugo-blog/content/presentation"
         :recursive t
         :base-extension "org\\|css\\|js\\|png\\|jpg\\|gif\\|pdf"
         :html-extension "html"
         :headline-levels 4
         :publishing-function org-reveal-export-to-html)))


(defun directory-files-recursive (directory match maxdepth)
  "List files in DIRECTORY and in its sub-directories.
Return files that match the regular expression MATCH. Recurse only
to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))
          ;; recurse only if necessary
          (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1))))
          (setq files-list (cons f files-list)))
         (t)))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

(defun org-tangle-all ()
  "Tangle all the Org-mode files under the current file's directory.
Returns the list of tangled files."
  (mapcar (lambda (f)
            (when (not (file-directory-p f))
              (org-babel-tangle-file f)))
          (directory-files-recursive (file-name-directory (buffer-file-name)) "\\.org$" 20)))

;; PlantUML
(setq org-plantuml-jar-path (expand-file-name "/usr/local/Cellar/plantuml/1.2021.16/libexec/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(use-package htmlize
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capturing Knowledge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.org/gtd.org" "Tasks")
	 "* TODO %?\n %i\n %a")
	("j" "Journal" entry (file+datetree "~/.org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

;; org-roam
(setq org-roam-v2-ack t)
(use-package org-roam)
(setq org-roam-directory "~/.org/roam/")
(if (not (file-directory-p org-roam-directory))
    (make-directory org-roam-directory))
(setq org-roam-db-location "~/.org/roam/org-roam.db")
(setq org-roam-index-file "~/.org/roam/index.org")
(add-hook 'after-init-hook 'org-roam-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK n FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts, styles, sizes for the headlines, tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-bullets)

;;(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Picked from http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;; and https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(let* ((variable-tuple
        (cond ((x-list-fonts "FiraMono Nerd Font")  '(:font "FiraMono Nerd Font"))
	      ((x-list-fonts "Fira Code")           '(:font "Fira Code"))
              ((x-list-fonts "Lucida Grande")       '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")             '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")        '(:family "Sans Serif"))
              (nil (warn "Cannot find any of the required fonts."))))
       (topline            `(:inherit fixed-pitch :weight ultra-bold :foreground "yellow" :underline nil))
       (headline           `(:inherit fixed-pitch :weight semi-bold :foreground "white")))

  (custom-theme-set-faces
   'user
  ;; `(org-tag ((t (,@topline ,@variable-tuple :foreground "yellow" :background "#333" :underline t))))
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
   `(org-level-1 ((t (,@topline ,@variable-tuple :height 1.4))))
   `(org-document-title ((t (,@headline ,@variable-tuple :underline nil))))))

;; Faces for other (non-title) elements
(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch :height 1.2))))
   '(org-date ((t (:inherit fixed-pitch :weight bold :foreground "yellow" :background "#222" :underline nil))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "#ffeeaa" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#a3d5c8"))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; From https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode 1)
	    (subword-mode 1)
	    (turn-on-visual-line-mode)))


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(provide 'init-org)
;;; init-org.el ends here
