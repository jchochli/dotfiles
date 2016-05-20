(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq user-full-name "James Chochlinski")
(setq user-mail-address "jchochli@xpzen.com")
;; disable vc-git
(setq vc-handled-backends ())
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq message-log-max 16384)
(setq debug-on-error t)
(setq org-log-done t)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(global-auto-revert-mode t)

(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])

;; eldoc
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default 'truncate-lines t)
(setq magit-last-seen-setup-instructions "1.4.0")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq load-prefer-newer t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(require 'use-package)

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

(use-package command-log-mode :ensure t :defer t)
(use-package f :ensure t)
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package seq :ensure t)
(use-package bug-hunter  :ensure t  :defer t)
(use-package visual-regexp  :ensure t  :defer t)
(use-package auto-complete :ensure t)
(use-package f  :ensure t)
(use-package s  :ensure t)
(use-package flycheck  :ensure t)
(use-package misc-cmds :ensure t)
(use-package ag :ensure t)
(use-package puml-mode :ensure t)
(use-package restclient :ensure t)
(use-package ob-restclient :ensure t)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)
  (setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "DELEGATED")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (emacs-lisp . t)
     (ruby . t)
     (python . t)
     (js . t)
     (R . t)     
     (sh . t)
     (sql . t))))

;; Enable puml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))

(use-package skewer-mode
  :commands skewer-mode
  :ensure t
    :config (skewer-setup))

(use-package diminish
  :ensure t
  :init
  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))
  (diminish 'isearch-mode))

(use-package ggtags :ensure t
  :commands ggtags-mode)

(use-package ido   
  :ensure t
  :init  
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-use-faces nil)
    (setq ido-file-extensions-order '(".el" ".java" ".js" ".rb"))
    (add-to-list 'ido-ignore-files "\\.DS_Store"))
    (add-hook 'ido-setup-hook
            (lambda ()
              ;; Go straight home
              (define-key ido-file-completion-map
                (kbd "~")
                (lambda ()
                  (interactive)
                  (if (looking-back "/")
                      (insert "~/")
                    (call-interactively 'self-insert-command)))))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))

(use-package idomenu
  :ensure t
  :bind ("M-i" . idomenu))

(use-package smex
  :ensure t
  :init (smex-initialize)
    :bind ("M-x" . smex))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode 1))

(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package kanban :ensure t :defer t)
(use-package org-jira :ensure t :defer t)

;; (use-package yasnippet
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (use-package yasnippets)
;;     (yas-global-mode 1)
;;     (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package projectile
  ;;:load-path "~/Development/repos/elisp/projectile"
  :ensure t
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config  
  (projectile-global-mode))

(use-package projectile-speedbar
  :disabled t
  :ensure t
  :config
  (global-set-key (kbd "M-<f2>") 'projectile-speedbar-open-current-buffer-in-tree))

(use-package switch-window  :ensure t  
  :bind ("C-x o" . switch-window))

(use-package clojure-mode
  :ensure t
  :init  
  (setq projectile-completion-system 'ido)
  (add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
  :config
  (rename-modeline "clojure-mode" clojure-mode "λ")  
  (use-package align-cljlet
    :ensure t
    :bind ("C-! a a" . align-cljlet)))

;;(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config
  (cljr-add-keybindings-with-prefix "C-!"))

(use-package paredit  :ensure t  
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(use-package cider  :ensure t
  :init  
  (setq cider-words-of-inspiration '("NREPL is ready!!"))
  ;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)  
  :config (defalias 'cji 'cider-jack-in))

(use-package rainbow-mode  :ensure t  
  :commands rainbow-mode)

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
    (bind-key "C-c SPC" 'ace-jump-mode))

(use-package ace-window  :ensure t )
(use-package markdown-mode  :ensure t  )
(use-package yaml-mode  :ensure t  )
(use-package groovy-mode  :ensure t  )
(use-package undo-tree  :ensure t  )
(use-package restclient :ensure t)
(use-package pianobar :ensure t)

(use-package web-mode  :ensure t  
  :mode (("\\.html$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.xsl\\'" . web-mode)))

(setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))

;; JSP
(use-package crappy-jsp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsp$" . crappy-jsp-mode)))

(use-package nxml :ensure t)
(use-package macrostep :ensure t)

(use-package bookmark
  :ensure
  :config
  (use-package bookmark+))

(use-package browse-kill-ring+
  :ensure
  :commands browse-kill-ring)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :bind ("M-/" . company-complete)
  :init
  (global-company-mode 1)
  :config
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-d" . company-show-doc-buffer)
             ("<tab>" . company-complete)))

(use-package eclim
  ;;:ensure t
  :requires (eclim company-emacs-eclim company)
  ;;:load-path "~/Development/repos/elisp/emacs-eclim"
  :mode
  (("\\.java\\'" . eclim-mode)
   ("\\.jspx\\'" . eclim-mode))
  :commands (eclim-mode)
  :config
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (global-set-key (kbd "M-/") 'company-complete)
  (use-package company-emacs-eclim
    :requires company
    :config    
    (company-emacs-eclim-setup)))

(use-package eclimd  
  ;;:load-path "~/Development/repos/elisp/emacs-eclim"
  :commands start-eclimd)


;;(setq eclim-print-debug-messages t)

;; (require 'eclim)
;; ;;(global-eclim-mode)
;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)
;; (require 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)
;; (global-set-key (kbd "M-/") 'company-complete)

(use-package highlight-cl
  :ensure t
  :demand t
  :config  
  (add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
  (add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords))

(use-package guide-key
  :disabled t
  :ensure t
  :config  
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 1.0)
  (guide-key-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq-default tab-width 4)
(setq js-indent-level 4)
(setq jsx-indent-level 4)
(setq c-basic-indent 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(winner-mode 1)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(setq visible-bell t)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
;; (setq tramp-default-method "ssh")
(setq tramp-verbose 9)

(add-hook 'js2-mode-hook
          '(lambda ()
             (local-set-key "\C-x\C-e" 'js-send-last-sexp)
             (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
             (local-set-key "\C-cb" 'js-send-buffer)
             (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
             (local-set-key "\C-cl" 'js-load-file-and-go)))

(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)

;; keyboard bindings for lookup
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")    
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-x C-p") 'other-window-backward)
(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(use-package buffer-move :ensure t 
  :bind (("C-c b j" . buf-move-left)
         ("C-c b k" . buf-move-right)
         ("C-c b p" . buf-move-up)
         ("C-c b n" . buf-move-down)))

(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;; Move more quickly
(global-set-key (kbd "M-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "M-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(use-package dired)
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package ert-runner
  :defer t
  :ensure t
  :defer t)

(use-package overseer
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :bind (("M-z" . undo-tree-undo)
         ("M-s-z" . undo-tree-redo)))

(use-package ejc-sql
  :ensure t)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local ac-ignore-case t)
            (auto-complete-mode)))

(use-package elisp-lint
  :load-path "~/Development/repos/elisp/elisp-lint")

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

(bind-keys :prefix-map my-lisp-devel-map
           :prefix "C-c e"
           ("E" . elint-current-buffer)
           ("b" . do-eval-buffer)
           ("c" . cancel-debug-on-entry)
           ("d" . debug-on-entry)
           ("e" . toggle-debug-on-error)
           ("F" . emacs-lisp-byte-compile-and-load)
           ("f" . find-function)
           ("j" . emacs-lisp-mode)
           ("l" . find-library)
           ("m" . macrostep-expand)
           ("p" . eval-print-last-sexp)
           ("r" . do-eval-region)
           ("s" . scratch)
           ("z" . byte-recompile-directory))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun was-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))

(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (--each (f-directories package-user-dir)
    (unless (was-compiled-p it)
      (byte-recompile-directory it 0))))


(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
                (replace-match (string ?\C-j) nil t))))

(ensure-packages-compiled)

;; (load "server")
;; (unless (server-running-p)
;;   (server-start))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c m c") 'mc/edit-lines)



