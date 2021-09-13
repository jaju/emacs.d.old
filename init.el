;;; -*- lexical-binding: t; buffer-read-only: t -*-
;;; init.el --- The entry point
;;; Commentary:
;;; Code:

(defvar *emacsd-start-time* (current-time))
(defvar *user-name* (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(defvar *emacsd-system-name* (system-name))
(defvar *emacsd-version* "0.0.1-SNAPSHOT")

(message "Powering up version %s of your configuration, %s!" *emacsd-version* *user-name*)

(defvar *emacsd-dir*
  (file-name-directory (or load-file-name
                           (buffer-file-name))))
(add-to-list 'load-path (concat *emacsd-dir* "site-lisp"))

;; Trick copied from doom-emacs
(defun load-env-file (file)
  (if (null (file-exists-p file))
      (signal 'file-error (list "No env vars file exists " file ". Create one with the `env` command and store the output in " (concat *emacsd-dir* "env")))
    (when-let
        (env
         (with-temp-buffer
           (save-excursion
             (setq-local coding-system-for-read 'utf-8)
             (insert "\n")
             (insert-file-contents file))
           (save-match-data
             (when (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
               (setq
                env (split-string (buffer-substring (match-beginning 1) (point-max))
                                  "\n"
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

(load-env-file (concat *emacsd-dir* "env"))
;;(getenv "PATH")

(setq custom-file (concat *emacsd-dir* "custom.el"))
(setq native-comp-async-report-warnings-errors 'silent)
(require 'org)
(org-babel-load-file (expand-file-name "core.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
