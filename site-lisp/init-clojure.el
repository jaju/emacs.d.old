;;; clojure.el -- Clojure fun configuration
;;; Commentary:
;;; Code:

(use-package flycheck-clj-kondo
  :ensure t)
(use-package cider
  :ensure t)
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(defun pretty-fns ()
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
		    (0 (progn (compose-region (match-beginning 1) (match-end 1)
					      "ƒ")
			      nil))))))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'cider-cheatsheet)
     (add-hook 'clojure-mode-hook 'lsp)
     (add-hook 'clojurescript-mode-hook 'lsp)
     (add-hook 'clojure-mode-hook 'pretty-fns)
     (add-hook 'cider-repl-mode-hook
               '(lambda () (define-key cider-repl-mode-map (kbd "C-c M-b")
			     'cider-repl-clear-buffer)))))

;;; Parinfer - for Clojure and other LISPs
;;; Picked from https://github.com/DogLooksGood/parinfer-mode
;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (("C-," . painfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults
;;              pretty-parens
;;              ;; evil
;;              lispy
;;              paredit
;;              smart-tab
;;              smart-yank))
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

(defun bb-repl ()
  (interactive)
  (inf-clojure "bb"))

(provide 'clojure)
;;; clojure.el ends here
