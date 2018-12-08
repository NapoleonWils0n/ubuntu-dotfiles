; melpa rackages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)

; server start
(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; fixing elpy keybinding
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)
;; For elpy
(setq elpy-rpc-python-command "python3")
;; For interactive shell
(setq python-shell-interpreter "python3")

(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

(setq tramp-default-method "ssh")

(add-to-list 'backup-directory-alist
                  (cons tramp-file-name-regexp nil))

;Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "org-protocol-capture-html")

; hide start up screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
; hide toolbar
(tool-bar-mode -1)
; hide scrollbar
(scroll-bar-mode -1)

; visual line mode
(add-hook 'text-mode-hook 'visual-line-mode)

; backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

; change prompt from yes or no, to y or n
(fset 'yes-or-no-p 'y-or-n-p)

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
(require 'evil)
(evil-mode 1)

; org mode
(require 'org)
(require 'org-protocol)
(require 'org-capture)
(require 'org-protocol-capture-html)
(setq org-agenda-files '("~/org/"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

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
      (file+headline "~/org/todo.org" "tasks")
      (file "~/org/templates/tpl-todo.txt")
      :empty-lines-before 1)
      ("w" "web site" entry
      (file+olp "~/org/web.org" "web")
      (file "~/org/templates/tpl-web.txt")
       :empty-lines-before 1)))

; refile
(setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

; powerline-evil
(require 'powerline)
(powerline-default-theme)

; dont display time in mode line
(display-time-mode 0)

; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; mode line
; remove mail from mode line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mail-string "")
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file+headline "~/org/todo.org" "tasks")
      (file "~/org/templates/tpl-todo.txt")
      :empty-lines-before 1)
     ("w" "web site" entry
      (file+olp "~/org/web.org" "web")
      (file "~/org/templates/tpl-web.txt")
      :empty-lines-before 1))))
 '(package-selected-packages
   (quote
    (xclip ranger json-mode graphviz-dot-mode elpy powerline ox-pandoc markdown-mode magit git-auto-commit-mode evil-surround evil-leader emmet-mode))))

(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "yellow2"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :foreground "Black"
                    :background "yellow2"
                    :box nil)

; Prepare stuff for org-export-backends
(setq org-export-backends '(org md html latex icalendar odt ascii))

; org hide markup
(setq org-hide-emphasis-markers t)

; org spacing
;(setq org-cycle-separator-lines 1)

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

; org-babel graphviz
(org-babel-do-load-languages
'org-babel-load-languages
'((dot . t)
  (shell . t))) ; this line activates bash shell script

; magit 
(global-set-key (kbd "C-x g") 'magit-status)

; git-auto-commit-mode auto push to head
;(setq-default gac-automatically-push-p t)

; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

; ox-pandoc export
(setq org-pandoc-options-for-markdown '((atx-headers . t)))
(setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))

; ranger
;(ranger-override-dired-mode t)
