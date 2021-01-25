;;; init.el -- The entry point
;;; Commentary:
;;; Code:

(defvar *emacsd-start-time* (current-time))
(defvar *emacsd-system-name* (system-name))
(defvar *emacsd-version* "0.0.1-SNAPSHOT")
(defvar *emacsd-dir*
  (file-name-directory (or load-file-name
                           (buffer-file-name))))
(setq custom-file (concat *emacsd-dir* "custom.el"))
(require 'org)
(org-babel-load-file (expand-file-name "core.org" user-emacs-directory))
