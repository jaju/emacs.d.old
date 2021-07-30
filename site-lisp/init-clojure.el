;;; init-clojure.el -- Clojure fun configuration
;;; Commentary:
;;; Code:

(use-package flycheck-clj-kondo
  :ensure t)
(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))
(use-package inf-clojure
  :ensure t)
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))
(use-package clj-refactor)

(defun clj/pretty-fns ()
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
		    (0 (progn (compose-region (match-beginning 1) (match-end 1)
					      "ƒ")
			      nil))))))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'cider-cheatsheet)
     ;;(add-hook 'clojure-mode-hook #'lsp)
     ;;(add-hook 'clojurescript-mode-hook #'lsp)
     (add-hook 'clojure-mode-hook #'clj/pretty-fns)
     (add-hook 'cider-repl-mode-hook
               (lambda () (define-key cider-repl-mode-map (kbd "C-c M-b")
			    'cider-repl-clear-buffer)))))

(defun clj/bb-repl ()
  (interactive)
  (inf-clojure "bb"))

(add-to-list 'auto-mode-alist
	     '("\\.edn$" . clojure-mode))

(dolist (mode '(paredit-mode
		clojure-paredit-setup
		subword-mode
		eldoc-mode
		flycheck-mode))
  (add-hook 'clojure-mode-hook mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
