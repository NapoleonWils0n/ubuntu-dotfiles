;; emacs start up --------------------------------------------------------------------------

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)


; melpa packages --------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)


; backup directory --------------------------------------------------------------------------

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "org-protocol-capture-html")


;; tramp ssh config --------------------------------------------------------------------------

(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

(add-to-list 'backup-directory-alist
                  (cons tramp-file-name-regexp nil))


; setq --------------------------------------------------------------------------------------

;; dont backup files opened by sudo
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

;; tramp setq
(setq tramp-default-method "ssh")

; company auto complete
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(add-hook 'after-init-hook 'global-company-mode)

; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; For elpy
(setq elpy-rpc-python-command "python3")

;; For interactive shell
(setq python-shell-interpreter "python3")

; case insensitive search
(setq read-file-name-completion-ignore-case t)
(setq pcomplete-ignore-case t)

; place headers on the left
(setq markdown-asymmetric-header t)

; markdown preview using pandoc
(setq markdown-command "pandoc -f markdown -t html -s -S --mathjax --highlight-style=pygments -c ~/git/pandoc-css/pandoc.css")

; gfm mode
(setq auto-mode-alist (cons '("\\.mdt$" . gfm-mode) auto-mode-alist))

; fix tab in evil for org mode
(setq evil-want-C-i-jump nil)

; dont show images full size
(setq org-image-actual-width nil)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

; always follow symlinks
(setq vc-follow-symlinks t)

; dired directory listing options for ls
(setq dired-listing-switches "-ahl")

; dired hide long listing by default
(defun my-dired-mode-setup ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'my-dired-mode-setup)

; Toggle Hidden Files in Emacs dired with C-x M-o
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

; require --------------------------------------------------------------------------------------

; evil
(require 'evil)
(evil-mode 1)

; which key
(require 'which-key)
(which-key-mode)

; powerline-evil
(require 'powerline)
(powerline-default-theme)

; ob-async
(require 'ob-async)

; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

; xml folding
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

; org mode
(require 'org)
(require 'org-tempo)
(require 'org-protocol)
(require 'org-capture)
(require 'org-protocol-capture-html)
(setq org-agenda-files '("~/git/org/"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; org mode --------------------------------------------------------------------------------------

; org-capture
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

; org capture templates
(setq org-capture-templates
    '(("t" "todo" entry
      (file+headline "~/git/org/todo.org" "Tasks")
      (file "~/git/org/templates/tpl-todo.txt")
      :empty-lines-before 1)
      ("w" "web site" entry
      (file+olp "~/git/org/web.org" "sites")
      (file "~/git/org/templates/tpl-web.txt")
       :empty-lines-before 1)))

; refile
(setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

; Prepare stuff for org-export-backends
(setq org-export-backends '(org md html latex icalendar odt ascii))

; org hide markup
(setq org-hide-emphasis-markers t)

; org column spacing for tags
(setq org-tags-column 0)

; todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "IN-PROGRESS(p/!)" "WAITING(w@/!)" "|" "DONE(d@)")))
(setq org-log-done t)

; Fast Todo Selection - Changing a task state is done with C-c C-t KEY
(setq org-use-fast-todo-selection t)

; org todo logbook
(setq org-log-into-drawer t)

; org babel supress do you want to execute code message
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

; org-babel graphviz
(org-babel-do-load-languages
'org-babel-load-languages
'((dot . t)
  (shell . t))) ; this line activates bash shell script

(setq org-latex-minted-options
    '(("frame" "lines") ("linenos=true")) )
;(setq org-latex-listings 'minted)
(setq org-latex-listings 'minted
    org-latex-packages-alist '(("" "minted"))
    org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-minted-options
    '(("frame" "lines") ("linenos=true")) )

; org open files
(setq org-file-apps
     (quote
     ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.mkv\\'" . "mpv %s")
     ("\\.mp4\\'" . "mpv %s")
     ("\\.mov\\'" . "mpv %s")
     ("\\.png\\'" . "sxiv %s")
     ("\\.jpg\\'" . "sxiv %s")
     ("\\.jpeg\\'" . "sxiv %s")
     ("\\.pdf\\'" . default))))

; ox-pandoc export
(setq org-pandoc-options-for-markdown '((atx-headers . t)))
(setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))

; dont indent src block for export
(setq org-src-preserve-indentation t)


; general settings --------------------------------------------------------------------------------------

; change prompt from yes or no, to y or n
(fset 'yes-or-no-p 'y-or-n-p)

; dont display time in mode line
(display-time-mode 0)

; magit 
(global-set-key (kbd "C-x g") 'magit-status)


; add to list --------------------------------------------------------------------------------------

; dired hide aync output buffer
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))


; define key ---------------------------------------------------------------------------------------

; fixing elpy keybinding
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)


; add hook -----------------------------------------------------------------------------------------

; visual line mode
(add-hook 'text-mode-hook 'visual-line-mode)

; flycheck syntax linting
(add-hook 'sh-mode-hook 'flycheck-mode)


; custom --------------------------------------------------------------------------------------------

; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline nil)))))

; package-selected-packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (haskell-mode csv-mode ob-async flycheck git-auto-commit-mode powerline ox-pandoc markdown-mode magit evil-surround evil-leader emmet-mode elpy undo-tree which-key))))


; mpv functions --------------------------------------------------------------------------------------

;; open youtube links with mpv
(defun mpv-play-url (url &rest args)
  ""
  (interactive)
  (start-process "mpv" nil "mpv" url))

;; browse url open different browsers based on url
(setq browse-url-browser-function
  (quote
    (("youtu\\.?be" . mpv-play-url)
    ;; catch all
    ("." . browse-url-default-browser))))
