;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; general settings
(setq initial-scratch-message nil)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; no title bar
(add-to-list 'default-frame-alist '(undecorated . t))

;; opacity
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame 'alpha-background 90)))

;; You might also want to set it for the initial frame if the hook doesn't catch it
(when (and (boundp 'initial-frame) initial-frame)
  (set-frame-parameter initial-frame 'alpha-background 90))

;;Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.config/emacs/lisp/")

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)
