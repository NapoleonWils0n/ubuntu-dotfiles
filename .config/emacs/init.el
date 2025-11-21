;; ----------------------------------------------------------------------------------
;; emacs init.el - also using early-init.el
;; ----------------------------------------------------------------------------------

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; ----------------------------------------------------------------------------------
;; Bootstrap use-package
;; ----------------------------------------------------------------------------------

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t) ;; Automatically install packages if not present


;; ----------------------------------------------------------------------------------
;; melpa packages
;; ----------------------------------------------------------------------------------

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "elpa" package-archives)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t))

;; package initialize (use-package will handle installation and requiring)
(package-initialize)

;; ----------------------------------------------------------------------------------
;; doom-theme : use-package
;; ----------------------------------------------------------------------------------

;; doom themes
(use-package doom-themes)

;; ----------------------------------------------------------------------------------
;; modus-vivendi-tinted : load-theme
;; ----------------------------------------------------------------------------------

(load-theme 'modus-vivendi-tinted t)

;; ----------------------------------------------------------------------------------
;; general settings : setq
;; ----------------------------------------------------------------------------------

;; Save all tempfiles in ~/.config/emacs/backups
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))

;; auto save list
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; history
(setq savehist-file "~/.config/emacs/savehist")
(savehist-mode 1)

(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;; dont backup files opened by sudo or doas
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo" "doas"))))))))


;; save
(save-place-mode 1)         ;; save cursor position
(desktop-save-mode 0)       ;; dont save the desktop session
(global-auto-revert-mode 1) ;; revert buffers when the underlying file has changed

;; scrolling
(pixel-scroll-precision-mode 1)

;; version control
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)
(setq undo-tree-auto-save-history nil)

;; pinentry
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)

;; display time in mode line, hide load average
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)       ;; display time

;; change prompt from yes or no, to y or n
(setq use-short-answers t)

;; turn off blinking cursor
(setq blink-cursor-mode nil)

;; suppress large file prompt
(setq large-file-warning-threshold nil)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; case insensitive search
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

;; M-n, M-p recall previous mini buffer commands
(setq history-length 25)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; eww browser text width
(setq shr-width 80)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; side windows
(setq switch-to-buffer-obey-display-actions t)

;; hippie expand
(setq save-abbrevs 'silently)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; emacs 28 - dictionary server
(setq dictionary-server "localhost")

;; mpd host
(setq mpc-host "/home/djwilcox/.config/mpd/socket")

;; ----------------------------------------------------------------------------------
;; xkb fix for alt and super on mac osx
;; ----------------------------------------------------------------------------------

(setq x-alt-keysym 'meta)
(setq x-super-keysym 'meta)

;; ----------------------------------------------------------------------------------
;; TAB bar mode
;; ----------------------------------------------------------------------------------

(setq tab-bar-show 1)                     ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)      ;; hide close tab button
(setq tab-bar-new-button-show nil)        ;; hide new tab button
(setq tab-bar-new-tab-choice "*scratch*") ;; default tab scratch
(setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable) 
(setq tab-bar-close-tab-select 'recent)
(setq tab-bar-new-tab-to 'right)
(setq tab-bar-tab-hints nil)
(setq tab-bar-separator " ")
(setq tab-bar-auto-width-max '((100) 20))
(setq tab-bar-auto-width t)

;; Customize the tab bar format to add the global mode line string
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))

;; menubar in tab bar
(add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)

;; Turn on tab bar mode after startup
(tab-bar-mode 1)

;; tab bar menu bar button
(setq tab-bar-menu-bar-button "👿")

;; ----------------------------------------------------------------------------------
;; buffer list
;; ----------------------------------------------------------------------------------

;; display Buffer List in same window
(add-to-list 'display-buffer-alist
             '("^*Buffer List*" display-buffer-same-window))


;; mandatory, as the dictionary misbehaves!
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*" display-buffer-in-side-window
               (side . right)
               (window-width . 0.50)))

;; Man display in current buffer
(setq Man-notify-method 'bully)

;; ----------------------------------------------------------------------------------
;; functions
;; ----------------------------------------------------------------------------------

;; clear the kill ring
(defun clear-kill-ring ()
  "Clear the results on the kill ring."
  (interactive)
  (setq kill-ring nil))

;; reload init.el
(defun my-reload-init ()
  "reload init.el"
  (interactive)
  (load-file "~/.config/emacs/init.el"))

;; pinch - play urls with mpd
(defun pinch-clipboard ()
  "Send a url from the clipboard to mpd with pinch"
  (interactive)
  (let ((url (current-kill 0 t)))
    (start-process "pinch" nil "pinch" "-i" url)))

;; wayland clipboard
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

;; ----------------------------------------------------------------------------------
;; add-to-list
;; ----------------------------------------------------------------------------------

;; mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; exec-path add local bin directory
(add-to-list 'exec-path "~/bin")

;; ----------------------------------------------------------------------------------
;; add-hook
;; ----------------------------------------------------------------------------------

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; visual line mode
(add-hook 'text-mode-hook 'visual-line-mode)

;; h1 line mode
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; ----------------------------------------------------------------------------------
;; keymap-global-set
;; ----------------------------------------------------------------------------------

;; org-capture
(keymap-global-set "C-c c" 'org-capture)

;; press M-/ and invoke hippie-expand
(keymap-global-set "M-/" 'hippie-expand)

;; window-toggle-side-windows
(keymap-global-set "C-x x w" 'window-toggle-side-windows)

;; open dired side window
(keymap-global-set "C-x x s" 'my/window-dired-vc-root-left)

;; ----------------------------------------------------------------------------------
;; keymap-set
;; ----------------------------------------------------------------------------------

(keymap-set global-map "C-c h" 'iedit-mode)
(keymap-set global-map "C-c l" 'org-store-link)
(keymap-set global-map "C-c a" 'org-agenda)

;; mpv seek to position at point
(keymap-set global-map "C-x ," 'my/mpv-seek-to-position-at-point)

;; mpv dired embark
(with-eval-after-load 'embark
  (define-key embark-file-map "l" #'mpv-play-marked-files))

;; ----------------------------------------------------------------------------------
;; fonts
;; ----------------------------------------------------------------------------------

(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

;; ----------------------------------------------------------------------------------
;; set-face-attribute
;; ----------------------------------------------------------------------------------

;; Set the default pitch face
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height efs/default-variable-font-size :weight 'regular)

;; tab bar background
(set-face-attribute 'tab-bar nil
                    :foreground "#93a1a1")

;; active tab
(set-face-attribute 'tab-bar-tab nil
                    :foreground "#51AFEF")

;; inactive tab
(set-face-attribute 'tab-bar-tab-inactive nil
                    :foreground "grey50")

;; ----------------------------------------------------------------------------------
;; doom-modeline
;; ----------------------------------------------------------------------------------

(use-package doom-modeline
  :init
  (setq doom-modeline-icon t
        doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-time-icon nil
        doom-modeline-buffer-encoding nil
        doom-modeline-major-mode-icon t
        doom-modeline-battery t
        doom-modeline-vcs-icon t
        doom-modeline-vcs-bar-width 4
        doom-modeline-vcs-max-length 15)
  (display-battery-mode t)
  :config
  (doom-modeline-mode 1)
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs check battery time)))

;; ----------------------------------------------------------------------------------
;; evil
;; ----------------------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-i-jump nil)
  :config
  (evil-mode 1))


;; ----------------------------------------------------------------------------------
;; evil-collection
;; ----------------------------------------------------------------------------------

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  ;; dired use h and l
  (evil-collection-define-key 'normal 'dired-mode-map
    "e" 'dired-find-file
    "h" 'dired-up-directory
    "l" 'dired-find-file-mpv))

;; ----------------------------------------------------------------------------------
;; general packages : use-package
;; ----------------------------------------------------------------------------------

(use-package csv-mode)
(use-package evil-leader)
(use-package git-auto-commit-mode)
(use-package iedit)
(use-package nerd-icons)
(use-package nix-mode)
(use-package s)
(use-package shrink-path)
(use-package wgrep)
(use-package yaml-mode)
(use-package systemd)

;;----------------------------------------------------------------------------------
;; ob-sync
;;----------------------------------------------------------------------------------

(use-package ob-async
  :config
  ;; ob-async sentinel fix
  (defun no-hide-overlays (orig-fun &rest args)
    (setq org-babel-hide-result-overlays nil))
  (advice-add 'ob-async-org-babel-execute-src-block :before #'no-hide-overlays))

;;----------------------------------------------------------------------------------
;; which-key
;;----------------------------------------------------------------------------------

(use-package which-key
  :config
  (which-key-mode))

;;----------------------------------------------------------------------------------
;; undo-tree
;;----------------------------------------------------------------------------------

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;; ----------------------------------------------------------------------------------
;; Vertico
;; ----------------------------------------------------------------------------------

(use-package vertico
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode 1)
  ;; Add vertico keybindings here if they were from vertico-directory and are still desired.
  ;; C-j and C-k are usually default vertico navigation.
  ;; M-h for vertico-directory-up can be implemented as a custom function if needed.
  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    ;; Example for M-h if needed, requires defining a custom function or finding a vertico equivalent:
    ;; (define-key vertico-map (kbd "M-h") 'my-vertico-directory-up)
    ))

;; ----------------------------------------------------------------------------------
;; Marginalia
;; ----------------------------------------------------------------------------------

(use-package marginalia
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode 1))

;; ----------------------------------------------------------------------------------
;; Consult
;; ----------------------------------------------------------------------------------

(use-package consult
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer) ;; remap switch-to-buffer
  ("M-y" . consult-yank-pop) ;; Moved consult-yank-pop binding here
  :init
  (setq completion-in-region-function #'consult-completion-in-region
        enable-recursive-minibuffers t) ;; It lets you use a new minibuffer when you're in the minibuffer
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history))

;; ----------------------------------------------------------------------------------
;; Orderless
;; ----------------------------------------------------------------------------------

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles . (partial-completion))))))

;; ----------------------------------------------------------------------------------
;; Embark
;; ----------------------------------------------------------------------------------

(use-package embark
  :bind
  ([remap describe-bindings] . embark-bindings)
  ("C-," . embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (use-package embark-consult
    :after embark consult
    :hook (embark-collect-mode . consult-preview-at-point-mode))

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
  The which-key help message will show the type and value of the
  current target followed by an ellipsis if there are further
  targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

;; ----------------------------------------------------------------------------------
;; dired
;; ----------------------------------------------------------------------------------

;; dired hide long listing by default -- Define this function early and globally
(defun my-dired-mode-setup ()
  "Show less information in dired buffers."
  (dired-hide-details-mode 1))

;; Dired setq variables that are safe to set early
(setq dired-kill-when-opening-new-dired-buffer t
      dired-use-ls-dired t
      dired-listing-switches "-ahlv"
      dired-omit-mode t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-free-space nil
      dired-dwim-target t)

;; Configuration for built-in 'dired' and 'dired-x'
(with-eval-after-load 'dired
  (require 'dired-x) ;; Load dired-x functionalities

  ;; Set dired-omit-files after dired-x has loaded it
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-guess-shell-alist-user '(("\\.pdf$" "zathura")))

  ;; dired hide async output buffer
  (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

  ;; Dired hooks (moved here to ensure my-dired-mode-setup is defined)
  (add-hook 'dired-mode-hook 'my-dired-mode-setup)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  ;; media-thumbnail-dired-mode
  (define-key dired-mode-map (kbd "C-x m") 'media-thumbnail-dired-mode))


;; ------------------------------------------------------------------------------------------------
;; side-windows
;; ------------------------------------------------------------------------------------------------

;; dired-find-file-other-window 
;; bound to <S-return>, g O, <normal-state> <S-return>, <normal-state> g O

;; dired side window
(defun my/window-dired-vc-root-left ()
  (interactive)
  (let ((dir (if (eq (vc-root-dir) nil)
                 (dired-noselect default-directory)
               (dired-noselect (vc-root-dir)))))
    (display-buffer-in-side-window
     dir `((side . left)
           (slot . 0)
           (window-width . 0.20)
           (window-parameters . ((no-delete-other-windows . t)
                                 (mode-line-format . (""))))))))


;; ----------------------------------------------------------------------------------
;; media-thumbnail
;; ----------------------------------------------------------------------------------

;; use ffmpegthumbnailer to create thumbnails for videos

(use-package media-thumbnail
  :diminish media-thumbnail-dired-mode
  :commands (media-thumbnail-dired-mode)
  :config
  (setq media-thumbnail-image-width 480)
  (setq media-thumbnail-cache-dir 
        (file-name-concat temporary-file-directory "ffmpegthumbnailer/"))
  (setq media-thumbnail-dired-should-hide-details-fn #'ignore))


;; ----------------------------------------------------------------------------------
;; fd-dired
;; ----------------------------------------------------------------------------------

(use-package fd-dired
  :init
  (setq fd-dired-display-in-current-window nil))


;; ----------------------------------------------------------------------------------
;; async and dired-async (dired-async is part of the async package)
;; ----------------------------------------------------------------------------------

(use-package async
  :config
  ;; Enable dired-async-mode after the async package is loaded
  (dired-async-mode 1))

;; ----------------------------------------------------------------------------------
;; rip-grep
;; ----------------------------------------------------------------------------------

(use-package rg
  :config
  ;; rip-grep automatically switch to results buffer
  ;; https://github.com/dajva/rg.el/issues/142
  (advice-add 'rg-run :after
              #'(lambda (_pattern _files _dir &optional _literal _confirm _flags) (pop-to-buffer (rg-buffer-name)))))

;; ----------------------------------------------------------------------------------
;; tramp
;; ----------------------------------------------------------------------------------

(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        tramp-allow-unsafe-temporary-files t)
  :config
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")))
  ;; set tramp shell to bash to avoid zsh problems
  (setenv "SHELL" "/bin/sh")
  (add-to-list 'tramp-backup-directory-alist
               (cons tramp-file-name-regexp nil))
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

;; ----------------------------------------------------------------------------------
;; org mode
;; ----------------------------------------------------------------------------------

(use-package org
  :defer t
  :init
  (setq org-agenda-files '("~/git/personal/org/")
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t
        org-tags-column 0
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-image-actual-width nil
        org-adapt-indentation nil
        org-export-async-debug t
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t
        org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex"))
        org-export-backends '(org md html latex icalendar odt ascii)
        org-todo-keywords
        '((sequence "TODO(t@/!)" "IN-PROGRESS(p/!)" "WAITING(w@/!)" "|" "DONE(d@)"))
        org-log-done t
        org-use-fast-todo-selection t
        org-log-into-drawer t
        org-file-apps
        (quote
         ((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.mkv\\'" . "mpv %s")
          ("\\.mp4\\'" . "mpv %s")
          ("\\.mov\\'" . "mpv %s")
          ("\\.pdf\\'" . default)))
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox")
  :config
  (require 'org-tempo)
  (require 'org-protocol)
  (require 'org-capture)
  (require 'org-faces)

  ;; org-timer covert seconds and milliseconds to hours, minutes, seconds, milliseconds
  (defun my/org-timer-secs-to-hms (s)
    "Convert integer S into hh:mm:ss.m
  If the integer is negative, the string will start with \"-\"."
    (let (sign m h)
      (setq x (number-to-string s)
            seconds (car (split-string x "[.]"))
            milliseconds (cadr (split-string x "[.]"))
            sec (string-to-number seconds)
            ms (string-to-number milliseconds))
      (setq sign (if (< sec 0) "-" "")
            sec (abs sec)
            m (/ sec 60) sec (- sec (* 60 m))
            h (/ m 60) m (- m (* 60 h)))
      (format "%s%02d:%02d:%02d.%02d" sign h m sec ms)))

  ;; org-timer covert hours, minutes, seconds, milliseconds to seconds, milliseconds
  (defun my/org-timer-hms-to-secs (hms)
    "Convert h:mm:ss string to an integer time.
  If the string starts with a minus sign, the integer will be negative."
    (if (not (string-match
              "\\([-+]?[0-9]+\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\([.]?[0-9]\\{0,3\\}\\)"
              hms))
        0
      (let* ((h (string-to-number (match-string 1 hms)))
             (m (string-to-number (match-string 2 hms)))
             (s (string-to-number (match-string 3 hms)))
             (ms (string-to-number (match-string 4 hms)))
             (sign (equal (substring (match-string 1 hms) 0 1) "-")))
        (setq h (abs h))
        (* (if sign -1 1) (+ s (+ ms (* 60 (+ m (* 60 h)))))))))

  ;; resize org headings
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka" :weight 'medium :height (cdr face)))

  (setq org-capture-templates
        '(("w" "web site" entry
           (file+olp "~/git/personal/bookmarks/bookmarks.org" "sites")
           "** [[%c][%^{link-description}]]"
           :empty-lines-after 1)
          ("v" "video url" entry
           (file+olp "~/git/personal/bookmarks/video.org" "links")
           "** [[video:%c][%^{link-description}]]"
           :empty-lines-after 1)))

  ;; refile
  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))

  (custom-set-faces
   '(org-link ((t (:inherit link :underline nil)))))

  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))

  ;; org-babel shell script
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (python . t)
     (dot . t)))

  ;; yank-media--registered-handlers org mode
  (setq yank-media--registered-handlers '(("image/.*" . #'org-mode--image-yank-handler)))

  ;; org mode image yank handler
  (yank-media-handler "image/.*" #'org-mode--image-yank-handler)

  ;; org-mode insert image as file link from the clipboard
  (defun org-mode--image-yank-handler (type image)
    (let ((file (read-file-name (format "Save %s image to: " type))))
      (when (file-directory-p file) (user-error "%s is a directory"))
      (when (and (file-exists-p file) (not (yes-or-no-p (format "%s exists; overwrite?" file)))) (user-error "%s exists"))
      (with-temp-buffer (set-buffer-multibyte nil) (insert image) (write-region (point-min) (point-max) file))
      (insert (format "[[file:%s]]\n" (file-relative-name file))))))

;; ----------------------------------------------------------------------------------
;; org tree slide
;; ----------------------------------------------------------------------------------

(use-package org-tree-slide
  :init
  (setq org-tree-slide-header nil
        org-tree-slide-activate-message "Presentation started"
        org-tree-slide-deactivate-message "Presentation finished"
        org-tree-slide-slide-in-effect t
        org-tree-slide-breakcrumbs " // "
        org-tree-slide-heading-emphasis nil
        org-tree-slide-slide-in-blank-lines 2
        org-tree-slide-indicator nil)
  :hook (org-tree-slide-play . my/presentation-setup)
  (org-tree-slide-stop . my/presentation-end)
  :config
  ;; presentation start
  (defun my/presentation-setup ()
    (setq-local mode-line-format nil)
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block))))

  ;; presentation end
  (defun my/presentation-end ()
    (doom-modeline-set-modeline 'main)
    (setq-local face-remapping-alist '((default fixed-pitch default)))
    (setq-local face-remapping-alist '((default variable-pitch default))))

  ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; make #+ lines invisible during presentation
  (defvar my-hide-org-meta-line-p nil)
  (defun my-hide-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p t)
    (set-face-attribute 'org-meta-line nil :foreground (face-attribute 'default :background)))
  (defun my-show-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p nil)
    (set-face-attribute 'org-meta-line nil :foreground nil))
  (defun my-toggle-org-meta-line ()
    (interactive)
    (if my-hide-org-meta-line-p (my-show-org-meta-line) (my-hide-org-meta-line))))

;; ----------------------------------------------------------------------------------
;; mpv.el 
;; ----------------------------------------------------------------------------------

(use-package mpv
  :init
  ;; mpv-default-options play fullscreen on second display
  (setq mpv-default-options '("--fs" "--fs-screen-name=DP-3"))


  :config
  ;; create a video: link type that opens a url using mpv-play-remote-video
  (org-link-set-parameters "video"
                           :follow #'mpv-play-remote-video
                           :store #'org-video-store-link)
  
  
  ;; org video store link
  (defun org-video-store-link ()
    "Store a link to a video url."
    (org-link-store-props
     :type "video"
     :link link
     :description description))
  
  
  ;; mpv-play-remote-video
  (defun mpv-play-remote-video (url &rest args)
    "Start an mpv process playing the video stream at URL."
    (interactive)
    (unless (mpv--url-p url)
      (user-error "Invalid argument: `%s' (must be a valid URL)" url))
    (if (not mpv--process)
        ;; mpv isnt running play file
        (mpv-start url)
      ;; mpv running append file to playlist
      (mpv--playlist-append url)))
  
  
  ;; mpv-play-clipboard - play url from clipboard
  (defun mpv-play-clipboard ()
    "Start an mpv process playing the video stream at URL."
    (interactive)
    (let ((url (current-kill 0 t)))
      (unless (mpv--url-p url)
        (user-error "Invalid argument: `%s' (must be a valid URL)" url))
      (if (not mpv--process)
          ;; mpv isnt running play file
          (mpv-start url)
        ;; mpv running append file to playlist
        (mpv--playlist-append url))))
  
  
  ;; create a mpv: link type that opens a file using mpv-play
  (defun org-mpv-complete-link (&optional arg)
    (replace-regexp-in-string
     "file:" "mpv:"
     (org-link-complete-file arg)
     t t))
  (org-link-set-parameters "mpv"
                           :follow #'mpv-play :complete #'org-mpv-complete-link)
  
  ;; M-RET will insert a new item with the timestamp of the current playback position
  (defun my:mpv/org-metareturn-insert-playback-position ()
    (when-let ((item-beg (org-in-item-p)))
      (when (and (not org-timer-start-time)
                 (mpv-live-p)
                 (save-excursion
                   (goto-char item-beg)
                   (and (not (org-invisible-p)) (org-at-item-timer-p))))
        (my/mpv-insert-playback-position t))))
  (add-hook 'org-metareturn-hook #'my:mpv/org-metareturn-insert-playback-position)


  ;; video and audio mime types
  (defvar supported-mime-types
    '("video/quicktime"
      "video/x-matroska"
      "video/mp4"
      "video/webm"
      "video/x-m4v"
      "video/x-msvideo"
      "audio/x-wav"
      "audio/mpeg"
      "audio/x-hx-aac-adts"
      "audio/mp4"
      "audio/flac"
      "audio/ogg"))
  
  ;; subr-x
  (load "subr-x")
  
  ;; get files mime type
  (defun get-mimetype (filepath)
    (string-trim
     (shell-command-to-string (concat "file -b --mime-type "
                                      (shell-quote-argument filepath)))))
  
  ;; dired-find-file-mpv
  (defun dired-find-file-mpv ()
    "Start an mpv process playing the file at PATH append subsequent files to the playlist"
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (member (get-mimetype file) supported-mime-types)
          (mpv-play-dired file)
        (dired-find-file))))


  ;; mpv play dired marked files
  (defun mpv-play-marked-files ()
    "Play marked files with mpv"
    (interactive)
    (mapc 'mpv-play-dired (dired-get-marked-files nil nil nil t)))


  ;; frame step forward
  (defun mpv-frame-step ()
    "Step one frame forward."
    (interactive)
    (mpv--enqueue '("frame-step") #'ignore))
  
  
  ;; frame step backward
  (defun mpv-frame-back-step ()
    "Step one frame backward."
    (interactive)
    (mpv--enqueue '("frame-back-step") #'ignore))
  
  
  ;; mpv take a screenshot
  (defun mpv-screenshot ()
    "Take a screenshot"
    (interactive)
    (mpv--enqueue '("screenshot") #'ignore))
  
  
  ;; mpv show osd
  (defun mpv-osd ()
    "Show the osd"
    (interactive)
    (mpv--enqueue '("set_property" "osd-level" "3") #'ignore))
  
  
  ;; add a newline in the current document
  (defun end-of-line-and-indented-new-line ()
    (interactive)
    (end-of-line)
    (newline-and-indent))


  ;; mpv insert playback position
  (defun my/mpv-insert-playback-position (&optional arg)
    "Insert the current playback position at point.

  When called with a non-nil ARG, insert a timer list item like `org-timer-item'."
    (interactive "P")
    (let ((time (mpv-get-playback-position)))
      (funcall
       (if arg #'mpv--position-insert-as-org-item #'insert)
       (my/org-timer-secs-to-hms (float time)))))
  
  
  ;; seek to position
  (defun my/mpv-seek-to-position-at-point ()
    "Jump to playback position as inserted by `mpv-insert-playback-position'.

  This can be used with the `org-open-at-point-functions' hook."
    (interactive)
    (save-excursion
      (skip-chars-backward ":[:digit:]" (point-at-bol))
      (when (looking-at "[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}\\([.]?[0-9]\\{0,3\\}\\)"))
      (let ((secs (my/org-timer-hms-to-secs (match-string 0))))
        (when (>= secs 0)
          (mpv-seek secs)))))

  
  ;; mpv-play-dired
  (defun mpv-play-dired (path)
    "Start an mpv process playing the file at PATH append subsequent files to the playlist"
    (if (not mpv--process)
        ;; mpv isnt running play file
        (mpv-start (expand-file-name path))
      ;; mpv running append file to playlist
      (mpv--playlist-append (expand-file-name path))))
  ) ;; This is the final closing parenthesis for the entire (use-package mpv ...) block

;; ----------------------------------------------------------------------------------
;; emacs desktop notification center
;; ----------------------------------------------------------------------------------

;; start ednc-mode
(use-package ednc
  :init
  (ednc-mode 1)
  :config
  (defun show-notification-in-buffer (old new)
    (let ((name (format "Notification %d" (ednc-notification-id (or old new)))))
      (with-current-buffer (get-buffer-create name)
        (if new (let ((inhibit-read-only t))
                  (if old (erase-buffer) (ednc-view-mode))
                  (insert (ednc-format-notification new t))
                  (pop-to-buffer (current-buffer)))
          (kill-buffer)))))
  
  
  ;; notifications hook
  (add-hook 'ednc-notification-presentation-functions
            #'show-notification-in-buffer)
  
  ;; open notifications in side window
  (add-to-list 'display-buffer-alist
               '("^Notification *" display-buffer-in-side-window
                 (side . right)
                 (window-width . 0.50)))
  
  ;; ednc evil - normal mode
  (defun noevil ()
    (evil-define-key 'normal ednc-view-mode-map "d" 'ednc-dismiss-notification)
    (evil-define-key 'normal ednc-view-mode-map (kbd "RET") 'ednc-invoke-action)
    )
  (add-hook 'ednc-view-mode-hook 'noevil))

;; ----------------------------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------------------------

(use-package hydra
  :after mpv 
  :config
  ;; Define hydra-mpv
  (defhydra hydra-mpv (:hint nil)
    "
    ^Seek^                  ^Actions^               ^General^                       ^Playlists^
    ^^^^^^^^-----------------------------------------------------------------------------------------------------------
    _h_: seek back -5       _,_: back frame         _i_: insert playback position   _n_: next item in playlist
    _j_: seek back -60      _._: forward frame      _m_: insert a newline           _p_: previous item in playlist
    _k_: seek forward 60    _SPC_: pause            _s_: take a screenshot          _e_: jump to playlist entry
    _l_: seek forward 5     _q_: quit mpv           _o_: show the osd               _r_: remove playlist entry
    ^
    "
    ("h" mpv-seek-backward "-5")
    ("j" mpv-seek-backward "-60")
    ("k" mpv-seek-forward "60")
    ("l" mpv-seek-forward "5")
    ("," mpv-frame-back-step)
    ("." mpv-frame-step)
    ("SPC" mpv-pause)
    ("q" mpv-kill)
    ("i" my/mpv-insert-playback-position)
    ("m" end-of-line-and-indented-new-line)
    ("s" mpv-screenshot)
    ("o" mpv-osd)
    ("n" mpv-playlist-next)
    ("p" mpv-playlist-prev)
    ("e" mpv-jump-to-playlist-entry)
    ("r" mpv-remove-playlist-entry))

  ;; Define hydra-emacs
  (defhydra hydra-emacs (:hint nil :exit t)
    "
    ^Actions^
    ^^^^^^^^--------------
    _m_: mpv clipboard
    _p_: pinch url
    ^
    "
    ("m" mpv-play-clipboard)
    ("p" pinch-clipboard))

  ;; Define hydra-nested
  (defvar hydra-stack nil)

  (defhydra hydra-nested (:exit t)
    ("e" hydra-emacs/body "emacs" :column "hydra")
    ("m" hydra-mpv/body "mpv" :column "hydra")
    ("q" nil "quit"))

  ;; Set global keybinding
  (global-set-key (kbd "C-a") 'hydra-nested/body))

;; ----------------------------------------------------------------------------------
;; google-translate
;; ----------------------------------------------------------------------------------

(use-package google-translate
  :config
  (setq google-translate-display-buffer-action
        '(pop-to-buffer-same-window)))

;; ----------------------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------------------

(use-package magit
  :init
  (setenv "SSH_AUTH_SOCK" "/run/user/1000/gcr/ssh"))

;; ----------------------------------------------------------------------------------
;; markdown mode
;; ----------------------------------------------------------------------------------

(use-package markdown-mode
  :commands (markdown-mode gfm-mode) ; Define mode functions to autoload the package
  :mode (("\\.md\\'" . markdown-mode) ; Use markdown-mode for *.md files
         ("\\.markdown\\'" . markdown-mode) ; Use markdown-mode for *.markdown files
         ("README\\.md\\'" . gfm-mode)) ; Use gfm-mode for README.md files
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (setq markdown-command "pandoc")
  )

;; ----------------------------------------------------------------------------------
;; treesitter
;; ----------------------------------------------------------------------------------

(require 'treesit)

;; Use `major-mode-remap-alist` for languages that have a Treesitter major mode.
;; M-x treesit-install-language-grammar bash
(add-to-list
 'treesit-language-source-alist
 '(bash "https://github.com/tree-sitter/tree-sitter-bash.git" "v0.20.1"))

(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)))

;; treesitter explore open in side window
(add-to-list 'display-buffer-alist
   '("^*tree-sitter explorer *" display-buffer-in-side-window
     (side . right)
     (window-width . 0.50)))

;; ----------------------------------------------------------------------------------
;; Snippets (YASnippet)
;; ----------------------------------------------------------------------------------

(use-package yasnippet
  :init
  ;; Enable YASnippet globally for all modes.
  (yas-global-mode 1)
  :config
  ;; Load the built-in snippets after the package is ready.
  (yas-reload-all)

  ;; Optional: Set the key for manually expanding a snippet (if TAB is taken)
  ;; By default, TAB usually works unless another minor mode takes it.
  ;; If you need a separate key, uncomment the line below.
  ;; (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
  )

;; Install the massive collection of community snippets
(use-package yasnippet-snippets
  :after yasnippet)


;; ----------------------------------------------------------------------------------
;; YASnippet Completion-At-Point Function (CAPF)
;; ----------------------------------------------------------------------------------

(use-package yasnippet-capf
  ;; Ensure it loads after YASnippet and Cape (if Cape is being used to combine CAPFs)
  :after (yasnippet cape)
  :config
  ;; This function is the one that actually works!
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; ----------------------------------------------------------------------------------
;; corfu
;; ----------------------------------------------------------------------------------

(use-package corfu
  ;; Install the package if not found
  :init
  ;; Enable Corfu globally.
  (global-corfu-mode)

  :custom
  ;; Optional: Enable auto-completion after typing 0 characters (as-you-type)
  (corfu-auto t)
  ;; Optional: Set the delay before the popup appears to 0.1 seconds
  (corfu-auto-delay 0.1))

;; corfu complete
(with-eval-after-load 'evil
  ;; Bind C-. to the standard Emacs completion command, which Corfu is designed to use.
  (define-key evil-normal-state-map (kbd "C-.") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-.") 'completion-at-point)
  (define-key evil-visual-state-map (kbd "C-.") 'completion-at-point)
  (define-key evil-motion-state-map (kbd "C-.") 'completion-at-point))

;; Set the global binding as well for non-Evil buffers
(keymap-global-set "C-." 'completion-at-point)

;; ----------------------------------------------------------------------------------
;; cape
;; ----------------------------------------------------------------------------------

(use-package cape
  :config
  ;; Add a list of cape completion functions to the standard Emacs CAPF
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )

;; ----------------------------------------------------------------------------------
;; eglot
;; ----------------------------------------------------------------------------------

;; Configure Eglot to recognize and start the language servers.
(with-eval-after-load 'eglot
  ;; sh-mode (for shell scripts) uses 'bash-language-server' with a 'start' argument
  (add-to-list 'eglot-server-programs
               '(sh-mode "bash-language-server" "start"))

  ;; python-mode uses 'python-lsp-server', whose executable is typically 'pylsp'
  (add-to-list 'eglot-server-programs
               '(python-mode "pylsp")))

;; Automatically start Eglot when opening a relevant file
(add-hook 'sh-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

;; ----------------------------------------------------------------------------------
;; auth-source
;; ----------------------------------------------------------------------------------

(require 'auth-source)
(add-to-list 'auth-sources (expand-file-name ".authinfo" user-emacs-directory))

;; ----------------------------------------------------------------------------------
;; gptel
;; ----------------------------------------------------------------------------------

(use-package gptel
  :init
  ;; Enable tool use
  (setq gptel-use-tools t)
  (setq gptel-default-mode 'org-mode
        gptel-post-response-functions #'gptel-end-of-response
        gptel-expert-commands t)
  (require 'gptel-integrations) 
  :config
  (setq gptel-model 'gemini-2.5-flash
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
                        :stream t))
  

;; ----------------------------------------------------------------------------------
;; display the Gemini buffer in same window
;; ----------------------------------------------------------------------------------

  (add-to-list 'display-buffer-alist
               '("^*Gemini*" display-buffer-same-window))


;; ----------------------------------------------------------------------------------
;; gptel set org source blocks to use sh and not bash
;; ----------------------------------------------------------------------------------

  (defun my/gptel-fix-src-header (beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^#\\+begin_src bash" end t)
        (replace-match "#+begin_src sh"))))

  (add-hook 'gptel-post-response-functions #'my/gptel-fix-src-header)


;; ----------------------------------------------------------------------------------
;; gptel-custom-tools.el loaded from ~/.config/emacs/lisp/gptel-custom-tools.el
;; ----------------------------------------------------------------------------------

  (load "gptel-custom-tools")) ;; gptel use-package config closing parentheses


;; ----------------------------------------------------------------------------------
;; mcp server
;; ----------------------------------------------------------------------------------

(use-package mcp
  :after gptel
  :custom
  (mcp-hub-servers `(("mcp-nixos" . (
                                     :command "podman" ; <-- Use your container runtime
                                     :args ("run" "--rm" "-i" "ghcr.io/utensils/mcp-nixos")))
                     ("searxng" . ( ; General web search tool
                                   :command "podman"
                                   :args ("run" "-i" "--rm"
                                          "--network=host"
                                          "-e" "SEARXNG_URL=http://localhost:8080"
                                          "mcp-searxng:local")
                                   ))
                     )) ;; closing parentheses

  :config
  (require 'mcp-hub))

;; ----------------------------------------------------------------------------------
;; docker
;; ----------------------------------------------------------------------------------

(use-package docker
  :bind ("C-c d" . docker)
  :custom
  (docker-command "podman"))

;; ----------------------------------------------------------------------------------
;; dockerfile-mode
;; ----------------------------------------------------------------------------------

(use-package dockerfile-mode
  :custom
  (dockerfile-mode-command "podman"))

;; ----------------------------------------------------------------------------------
;; garbage collection
;; ----------------------------------------------------------------------------------

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
