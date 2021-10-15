;;; init.el --- Bootstrap Emacs Configuration from config.org
;;; Commentary:
;; This initialization script assumes a config.org in the Emacs home directory.
;; If one is present it will perform some basic package manager initialization
;; and bootstrap the configuration there.

;;; Code:

;; Default memory limit is crazy low.  Give some breathing room
(setq gc-cons-threshold (* 128 1000 1000)) ;; 128 Mb for now

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

;;; Set up package
(require 'package)

;; Add the package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))

;; With the configuration done, initialize the package manager
(package-initialize)

;;; Bootstrap use-package
;; use-package is used to configure the rest of the packages.
;; Only force refresh if we need one of the 3 packages bootstrapped
(unless (and (package-installed-p 'use-package)
			 (package-installed-p 'diminish))
  (package-refresh-contents))

;; Install use-package if needed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; No need to require until needed
(eval-when-compile
  (require 'use-package))

;; Make sure packages are present before trying to configure them
(setq use-package-always-ensure t)

;; Check if diminish is present, install if not
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

;; Check if bind-key is present, install if no
(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))

;; Require diminish and bind key before they're used in configuring packages
(require 'diminish)
(require 'bind-key)

;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;; End: init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode lsp-dart dart-mode cmake-mode ccls counsel-projectile projectile ivy-xref lsp-ui lsp-mode forge flycheck exec-path-from-shell org-bullets visual-fill-column evil-collection evil general which-key helpful ivy-rich counsel ivy treemacs rainbow-delimiters doom-modeline doom-themes all-the-icons no-littering auto-package-update diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
