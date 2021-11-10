;;; init.el --- Bootstrap Emacs Configuration from config.org

;;; Commentary:
;; This initialization script assumes a config.org in the Emacs home directory.
;; If one is present it will perform some basic package manager initialization
;; and bootstrap the configuration there.

;;; Code:

;; Define function for timing startup.  Useful for fine tuning.
(defun void/display-startup-time ()
  "Time Emacs startup"
  (message "Emacs loaded in %s with %d garbage collections."
		   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
		   gcs-done))

;; Add the timing function to the startup hook
(add-hook 'emacs-startup-hook #'void/display-startup-time)

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1) ;; Disable menu bars
  (tool-bar-mode -1) ;; Disable tool bars
  (scroll-bar-mode -1) ;; Disable Scroll bars
  (tooltip-mode -1)) ;; Disable tool tips

;; Start clean
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defalias 'org-file-name-concat #'file-name-concat)

;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))

;; Don't edit my init on me:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Done initializing, enable Garbage Colletion magic hack
(gcmh-mode 1)

;;; End: init.el
