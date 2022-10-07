;; emacs start up --------------------------------------------------------------------------

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

;; pinentry prompt in emacs
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)

;; dont backup files opened by sudo or doas
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo" "doas"))))))))


;; melpa packages --------------------------------------------------------------------------

;; package-selected-packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" default))
 '(package-selected-packages
   '(doom-themes openwith hydra mpv company csv-mode emmet-mode evil-collection evil-surround evil-leader flycheck git-auto-commit-mode haskell-mode iedit ob-async ox-pandoc powerline magit rg undo-tree which-key s))
 '(warning-suppress-types '((comp))))

;; require package
(require 'package)

;; package archive
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; package initialize
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)


;; general settings --------------------------------------------------------------------------------------

;; load theme
(add-hook 'after-init-hook (lambda () (load-theme 'doom-solarized-dark)))

;; font
(add-to-list 'default-frame-alist
             '(font . "Inconsolata 18"))

;; hide toolbar
(tool-bar-mode -1)

;; hide scrollbar
(scroll-bar-mode -1)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; h1 line mode
(global-hl-line-mode 1)
(set-face-background hl-line-face "#073042")

;; change prompt from yes or no, to y or n
;;(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t) ;; emacs 28

;; dont display time in mode line
(display-time-mode 0)

;; mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;;Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.config/emacs/lisp/")
(load "org-protocol-capture-html")

;; save cursor position
(save-place-mode 1)

;; turn off blinking cursor
(setq blink-cursor-mode nil)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; emacs 28 - dictionary server
(setq dictionary-server "dict.org")


;; require --------------------------------------------------------------------------------------

;; evil
(setq evil-want-keybinding nil)
(require 'evil)
(evil-collection-init)
(evil-mode 1)

;; which key
(require 'which-key)
(which-key-mode)

;; powerline-evil
(require 'powerline)
(powerline-default-theme)

;; ob-async
(require 'ob-async)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history nil)

;; org mode
(require 'org)
(require 'org-tempo)
(require 'org-protocol)
(require 'org-capture)
(require 'org-protocol-capture-html)
(setq org-agenda-files '("~/git/personal/org/"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; tramp
(require 'tramp)

;; openwith
(require 'openwith)
(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("mpg" "mpeg" "mp3" "mp4" "m4v"
                "avi" "wmv" "wav" "mov" "flv"
                "ogm" "ogg" "mkv" "webm"))
             "mpv"
             '(file))
       (list (openwith-make-extension-regexp
              '("xbm" "pbm" "pgm" "ppm" "pnm"
                "png" "gif" "bmp" "tif" "jpeg" "jpg" "webp"))
             "nsxiv -a"
             '(file))
       (list (openwith-make-extension-regexp
              '("pdf"))
             "zathura"
             '(file))))
(openwith-mode 1)

;; setq --------------------------------------------------------------------------------------

;; suppress large file prompt
(setq large-file-warning-threshold nil)

;; asynchronous tangle
(setq org-export-async-debug t)

;; tramp setq
(setq tramp-default-method "ssh")

;; company auto complete
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(add-hook 'after-init-hook 'global-company-mode)

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; case insensitive search
(setq read-file-name-completion-ignore-case t)
(setq pcomplete-ignore-case t)

;; fix tab in evil for org mode
(setq evil-want-C-i-jump nil)

;; dont show images full size
(setq org-image-actual-width nil)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;; always follow symlinks
(setq vc-follow-symlinks t)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; M-n, M-p recall previous mini buffer commands
(setq history-length 25)
(savehist-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)


;; tramp ssh config --------------------------------------------------------------------------

;; set tramp shell to sh to avoid zsh problems
(eval-after-load 'tramp '(setenv "SHELL" "/usr/bin/sh"))

(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

(add-to-list 'backup-directory-alist
                  (cons tramp-file-name-regexp nil))


;; define key ---------------------------------------------------------------------------------------

;; iedit
(define-key global-map (kbd "C-c o") 'iedit-mode)


;; add hook -----------------------------------------------------------------------------------------

;; visual line mode
(add-hook 'text-mode-hook 'visual-line-mode)

;; flycheck syntax linting
(add-hook 'sh-mode-hook 'flycheck-mode)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;; dired --------------------------------------------------------------------------------------

;; kill the current buffer when selecting a new directory to display
(setq dired-kill-when-opening-new-dired-buffer t)

;; dired directory listing options for ls
(setq dired-listing-switches "-ahlv")

;; dired hide long listing by default
(defun my-dired-mode-setup ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'my-dired-mode-setup)

;; Toggle Hidden Files in Emacs dired with C-x M-o
(require 'dired-x)
;; hide dotfiles and firefox.tmp
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|firefox.tmp$"))

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; recursive delete and copy
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; dired hide aync output buffer
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; dired use h and l
(evil-collection-define-key 'normal 'dired-mode-map
    "e" 'dired-find-file
    "h" 'dired-up-directory
    "l" 'dired-find-file)

;; dired dwim
(setq dired-dwim-target t)

;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; revert buffers when the underlying file has changed
(global-auto-revert-mode 1)


;; magit -------------------------------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)

;; delete magit buffers
(defun kill-magit-diff-buffer-in-current-repo (&rest _)
      "Delete the magit-diff buffer related to the current repo"
      (let ((magit-diff-buffer-in-current-repo (magit-mode-get-buffer 'magit-diff-mode)))
        (kill-buffer magit-diff-buffer-in-current-repo)))
    ;;
    ;; When 'C-c C-c' or 'C-c C-l' are pressed in the magit commit message buffer,
    ;; delete the magit-diff buffer related to the current repo.
    ;;    
    (add-hook 'git-commit-setup-hook
              (lambda ()
                (add-hook 'with-editor-post-finish-hook #'kill-magit-diff-buffer-in-current-repo
                          nil t)
                (add-hook 'with-editor-post-cancel-hook #'kill-magit-diff-buffer-in-current-repo
                          nil t)))


;; org mode --------------------------------------------------------------------------------------

;; org-capture
(global-set-key "\C-cc" 'org-capture)

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

;; org capture templates
(setq org-capture-templates
    '(("t" "todo" entry
      (file+headline "~/git/personal/org/todo.org" "Tasks")
      (file "~/git/personal/org/templates/tpl-todo.txt")
      :empty-lines-before 1)
      ("w" "web site" entry
      (file+olp "~/git/personal/org/web.org" "sites")
      (file "~/git/personal/org/templates/tpl-web.txt")
       :empty-lines-before 1)))

;; refile
(setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; Prepare stuff for org-export-backends
(setq org-export-backends '(org md html latex icalendar odt ascii))

;; org hide markup
(setq org-hide-emphasis-markers t)

;; org column spacing for tags
(setq org-tags-column 0)

;; todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "IN-PROGRESS(p/!)" "WAITING(w@/!)" "|" "DONE(d@)")))
(setq org-log-done t)

;; Fast Todo Selection - Changing a task state is done with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;; org todo logbook
(setq org-log-into-drawer t)

;; org babel supress do you want to execute code message
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

; org-babel shell script
(org-babel-do-load-languages
'org-babel-load-languages
'((shell . t))) 

(setq org-latex-minted-options
    '(("frame" "lines") ("linenos=true")) )
;;(setq org-latex-listings 'minted)
(setq org-latex-listings 'minted
    org-latex-packages-alist '(("" "minted"))
    org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-minted-options
    '(("frame" "lines") ("linenos=true")) )

;; org open files
(setq org-file-apps
     (quote
     ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.mkv\\'" . "mpv %s")
     ("\\.mp4\\'" . "mpv %s")
     ("\\.mov\\'" . "mpv %s")
     ("\\.png\\'" . "nsxiv %s")
     ("\\.jpg\\'" . "nsxiv %s")
     ("\\.jpeg\\'" . "nsxiv %s")
     ("\\.pdf\\'" . default))))

;; ox-pandoc export
(setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))

;; dont indent src block for export
(setq org-src-preserve-indentation t)

;; org mode copy url from org link
(fset 'getlink
      (lambda (&optional arg) 
        "Keyboard macro." 
        (interactive "p") 
        (kmacro-exec-ring-item (quote ("\C-c\C-l\C-a\C-k\C-g" 0 "%d")) arg)))

(define-key org-mode-map (kbd "C-c p") #'getlink)

;; org src to use the current window
(setq org-src-window-setup 'current-window)

;; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline nil)))))


;; mpv.el --------------------------------------------------------------------------------------------------

(org-link-set-parameters "mpv" :follow #'mpv-play)
(defun org-mpv-complete-link (&optional arg)
  (replace-regexp-in-string
   "file:" "mpv:"
   (org-link-complete-file arg)
   t t))

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

;; mpv insert playback position
(with-eval-after-load 'mpv
  (defun my/mpv-insert-playback-position (&optional arg)
    "Insert the current playback position at point.
  
  When called with a non-nil ARG, insert a timer list item like `org-timer-item'."
    (interactive "P")
    (let ((time (mpv-get-playback-position)))
      (funcall
       (if arg #'mpv--position-insert-as-org-item #'insert)
       (my/org-timer-secs-to-hms (float time))))))


;; seek to position
(with-eval-after-load 'mpv
  (defun my/mpv-seek-to-position-at-point ()
    "Jump to playback position as inserted by `mpv-insert-playback-position'.
  
  This can be used with the `org-open-at-point-functions' hook."
    (interactive)
    (save-excursion
      (skip-chars-backward ":[:digit:]" (point-at-bol))
      (when (looking-at "[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}\\([.]?[0-9]\\{0,3\\}\\)"))
        (let ((secs (my/org-timer-hms-to-secs (match-string 0))))
          (when (>= secs 0)
            (mpv-seek secs))))))

;; mpv seek to position at point
(define-key global-map (kbd "C-x ,") 'my/mpv-seek-to-position-at-point)

;; org-timer milliseconds for mpv ----------------------------------------------------------

;; org-timer covert seconds and milliseconds to hours, minutes, seconds, milliseconds
(with-eval-after-load 'org-timer
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
      (format "%s%02d:%02d:%02d.%02d" sign h m sec ms))))

;; org-timer covert hours, minutes, seconds, milliseconds to seconds, milliseconds
(with-eval-after-load 'org-timer
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
        (* (if sign -1 1) (+ s (+ ms (* 60 (+ m (* 60 h))))))))))

;; mpv commands -----------------------------------------------------------------------------------------

;; frame step forward
(with-eval-after-load 'mpv
  (defun mpv-frame-step ()
    "Step one frame forward."
    (interactive)
    (mpv--enqueue '("frame-step") #'ignore)))


;; frame step backward
(with-eval-after-load 'mpv
  (defun mpv-frame-back-step ()
    "Step one frame backward."
    (interactive)
    (mpv--enqueue '("frame-back-step") #'ignore)))


;; mpv take a screenshot
(with-eval-after-load 'mpv
  (defun mpv-screenshot ()
    "Take a screenshot"
    (interactive)
    (mpv--enqueue '("screenshot") #'ignore)))


;; mpv show osd
(with-eval-after-load 'mpv
  (defun mpv-osd ()
    "Show the osd"
    (interactive)
    (mpv--enqueue '("set_property" "osd-level" "3") #'ignore)))


;; add a newline in the current document
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))


;; hydra --------------------------------------------------------------------------------------------------

(defhydra hydra-mpv (global-map "<f2>")
  "
  ^Seek^                    ^Actions^                ^General^
  ^^^^^^^^---------------------------------------------------------------------------
  _h_: seek back -5         _,_: back frame          _i_: insert playback position
  _j_: seek back -60        _._: forward frame       _n_: insert a newline
  _k_: seek forward 60      _SPC_: pause             _s_: take a screenshot
  _l_: seek forward 5       _q_: quit mpv            _o_: show the osd
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
  ("s" mpv-screenshot)
  ("i" my/mpv-insert-playback-position)
  ("o" mpv-osd)
  ("n" end-of-line-and-indented-new-line))


;;open youtube links with mpv ------------------------------------------------------------------------------

(defun mpv-play-url (url &rest args)
  ""
  (interactive)
  (start-process "mpv" nil "mpv" url))

(setq browse-url-handlers
    '(("youtu\\.?be" . mpv-play-url)
    ("." . browse-url-default-browser)))


;; eww browser text width  ------------------------------------------------------------------------------

(setq shr-width 80)


;; garbage collection -----------------------------------------------------------------------------------

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
