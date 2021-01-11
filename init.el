(defvar *emacsd-start-time* (current-time))
(defvar *emacsd-system-name* (system-name))
(defvar *emacsd-version* "0.0.0-SNAPSHOT")
(defvar *emacsd-dir*
  (file-name-directory (or load-file-name 
                           (buffer-file-name))))
(setq custom-file (concat *emacsd-dir* "custom.el"))

(setq inhibit-startup-message t
      visible-bell t
      visual-line-mode t
      tab-width 2
      indent-tabs-mode nil
      tab-always-indent 'complete
      gc-cons-threshold 10000000
      read-process-output-max (* 1024 1024))
;; Turn off the following modes
(dolist (mode '(tooltip-mode menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(set-fringe-mode 10) ;; Shift a bit from the edges
(delete-selection-mode t) ;; Delete when beginning to type when text selected.
(show-paren-mode)

;; Mac-specific settings
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))


(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(set-face-attribute 'default nil :font "Fira Code" :height 180)
;; (load-theme 'modus-vivendi)
(load-theme 'wombat)

(defun load-env-file (file)
  (if (null (file-exists-p file))
      (signal 'file-error (list "No env vars file exists " file))
    (when-let
        (env
         (with-temp-buffer
           (save-excursion
             (setq-local coding-system-for-read 'utf-8)
             (insert "\0\n")
             (insert-file-contents file))
           (save-match-data
             (when (re-search-forward "\0\n *\\([^#= \n]*\\)=" nil t)
               (setq
                env (split-string (buffer-substring (match-beginning 1) (point-max))
                                  "\0\n"
                                  'omit-nulls))))))
      (setq-default
       process-environment
       (append (nreverse env)
               (default-value 'process-environment))
       exec-path
       (append (split-string (getenv "PATH") path-separator t)
               (list exec-directory))
       shell-file-name
       (or (getenv "SHELL")
           (default-value 'shell-file-name)))
      env)))

(add-to-list 'load-path (concat *emacsd-dir* "site-lisp"))
(load-env-file (concat *emacsd-dir* "env"))

;; Initialize package sources
(require 'package)

;; Set up our repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode) ;; show keystrokes. invoke clm/toggle-command-log-buffer
;; There is also a command named global-command-log-mode
(use-package diminish) ;; We know you exist. We value you. Best behind the scenes though.

(use-package doom-modeline) ;; a modern modeline
(doom-modeline-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package treemacs)

(use-package smex)
  (use-package swiper)
  (use-package counsel)
  (use-package ivy
    :diminish
    :config
    (ivy-mode 1))

  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(use-package which-key
  :config
  (which-key-mode))
(use-package yasnippet) ;; By default, lsp-mode turns on snippets
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :custom
  (lsp-diagnostics-modeline-mode :project))
(use-package lsp-ivy)
(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :after (ivy lsp-mode))
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(load "init-org")

(load "init-clojure")



(when (fboundp 'toggle-frame-fullscreen)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen))

(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
