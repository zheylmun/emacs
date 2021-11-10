;;; early-init.el --- lexical-binding: t; ---

;;; Commentary:
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.
;; This early init is based almost entirely on that of [[https://github.com/hlissner/doom-emacs][Doom Emacs]]

;;; Code:
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; TODO: Seems like this is probably what was causing the org-file-name-concat issue
;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)

;; Ensure Emacs is running out of the .emacs.d directory
(setq user-emacs-directory (file-name-directory load-file-name))

;;; early-init.el ends here
