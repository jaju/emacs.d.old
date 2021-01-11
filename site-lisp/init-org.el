;;; package --- Personal org setup
;;; Commentary:
;; Copyright (C) 2015 Ravindra R. Jaju
;; Author: Ravindra Jaju - https:/msync.org/

;;; Code:
;;;

(require 'org)

;; Places where the agenda files exist.
(setq org-directory "~/.org/")
(setq org-agenda-files '("~/.org/agenda"))
(setq org-log-done t) ;; This sets timestamps on tasks when finished.
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/.org/on-the-fly-notes.org")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "DELEGATED")  ;; Tasks
        (sequence "SCHEDULE" "|" "MEETING-OVER")            ;; Meetings
        (sequence "RAW" "REFINE" "|" "IGNORE" "RECORDED"))) ;; Ideas

;; BEGIN -- https://github.com/stuartsierra/dotfiles
;; Org-babel and Clojure
;;(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
;;(require 'cider)

;; reveal.js setup
;; (require 'ox-reveal)
;; (setq org-reveal-external-plugins
;;     '((animate . "{src: '%splugin/animate/animate.js', async: true, condition: function() { return !!document.body.classList; }}")
;;       (anything . "{src: '%splugin/animate/anything.js', async: true, condition: function() { return true; }}"))

;; Moar languages
;(require 'ob-js)
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
   (plantuml . t)))
(setq org-confirm-babel-evaluate nil)

;; More from http://fgiasson.com/blog/index.php/2016/04/05/using-clojure-in-org-mode-and-implementing-asynchronous-processing/
;(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
;(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)

;; END

;(require 'ox-publish)
;(with-eval-after-load 'ox
;  (require 'ox-hugo))

;(setq org-html-html5-fancy t
;      org-src-tab-acts-natively t
;      org-reveal-root "https://p.msync.org/reveal.js")

;(defvar hugo-base-dir "~/Projects/hugo-blog")

;; (define-skeleton org-post-skeleton
;;   "Inserts the right directives for hugo-orgmode blogging"
;;   "Title: "
;;   "#+HUGO_BASE_DIR: " hugo-base-dir "\n"
;;   "#+HUGO_SECTION: posts\n"
;;   "#+TITLE: " str "\n"
;;   "#+SUMMARY: \n"
;;   "#+DATE: " (now) "\n"
;;   "#+LASTMOD: " (now) "\n"
;;   "#+TAGS[]: \n"
;;   "#+PUBLISHED: false\n"
;;   "#+PROPERTY: header-args:clojure :exports source :results output :comments link :session *clojure-nrepl*\n"
;;   "#+PROPERTY: header-args:python :exports source :results output :comments link :session *python-dl*\n"
;;   "#+PROPERTY: header-args:bash :exports source :results output :comments link :session *shell*\n")


;; (define-skeleton org-note-skeleton
;;   "Inserts the right directives for hugo-orgmode blogging"
;;   "Title: "
;;   "#+HUGO_BASE_DIR: " hugo-base-dir "\n"
;;   "#+HUGO_SECTION: notes\n"
;;   "#+TITLE: " str "\n"
;;   "#+SUMMARY: \n"
;;   "#+DATE: " (now) "\n"
;;   "#+LASTMOD: " (now) "\n"
;;   "#+TAGS[]: \n"
;;   "#+PUBLISHED: false\n"
;;   "#+PROPERTY: header-args:clojure :exports source :results output :comments link :session *clojure-nrepl*\n"
;;   "#+PROPERTY: header-args:python :exports source :results output :comments link :session *python-dl*\n"
;;   "#+PROPERTY: header-args:bash :exports source :results output :comments link :session *shell*\n")

;; (setq org-publish-project-alist
;;       '(
;;         ("msync-notes"
;;          :base-directory "~/.org/msync/notes"
;;          :base-extension "org"
;;          :recursive t
;;          :publishing-function org-publish-attachment
;;          :publishing-directory "~/Projects/hugo-blog/content/notes")

;;         ("msync-notes-static"
;;          :base-directory "~/.org/msync/notes"
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|py\\|clj\\|cljs"
;;          :recursive t
;;          :publishing-directory "~/Projects/hugo-blog/content/notes"
;;          :publishing-function org-publish-attachment)

;;         ("msync"
;;          :components ("msync-notes" "msync-notes-static"))

;;         ("msync-presentation"
;;          :base-directory "~/.org/presentations"
;;          :publishing-directory "~/Projects/hugo-blog/content/presentation"
;;          :recursive t
;;          :base-extension "org\\|css\\|js\\|png\\|jpg\\|gif\\|pdf"
;;          :html-extension "html"
;;          :headline-levels 4
;;          :publishing-function org-reveal-export-to-html)))

(defun now ()
  "Insert the current timestamp at the cursor position."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%:z")))
(defun today ()
  "Insert the current timestamp at the cursor position."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a]")))
(define-key global-map (kbd "\C-xt") 'today)


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

;; org-roam
(use-package org-roam)
(setq org-roam-directory "~/.org/roam/")
(setq org-roam-db-location "~/.org/roam/org-roam.db")
(setq org-roam-index-file "~/.org/roam/index.org")
(add-hook 'after-init-hook 'org-roam-mode)

(add-hook 'org-mode-hook
	  (lambda () (subword-mode 1)))

;; PlantUML
(setq org-plantuml-jar-path (expand-file-name "/usr/local/Cellar/plantuml/1.2021.0/libexec/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(provide 'orgmode)
;;; orgmode.el ends here
