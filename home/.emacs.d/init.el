(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq user-full-name "James Chochlinski")
(setq user-mail-address "jchochli@xpzen.com")
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq message-log-max 16384)
(setq debug-on-error t)
(add-to-list 'exec-path "/usr/local/bin")

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;(eval-when-compile  (require 'use-package))
(setq use-package-verbose t)

(use-package company  :ensure t  :defer t
  :config 
  (add-hook 'after-init-hook 'global-company-mode))
;; -------------------------------------
;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   :commands company-mode
;;   :config
;;   ;; From https://github.com/company-mode/company-mode/issues/87
;;   ;; See also https://github.com/company-mode/company-mode/issues/123
;;   (defadvice company-pseudo-tooltip-unless-just-one-frontend
;;       (around only-show-tooltip-when-invoked activate)
;;     (when (company-explicit-action-p)
;;       ad-do-it))
;;   (use-package helm-company
;;     :ensure t
;;     :disabled t))
;; (use-package ag
;;   :ensure t
;;   :commands (ag ag-regexp)
;;   :init
;;   (use-package helm-ag
;;     :ensure t
;;     :commands helm-ag))

;; (use-package helm-grep
;;   :ensure t
;;   :commands helm-do-grep-1
;;   :bind (("M-s f" . my-helm-do-grep-r)
;;          ("M-s g" . my-helm-do-grep))
;;   :preface
;;   (defun my-helm-do-grep ()
;;     (interactive)
;;     (helm-do-grep-1 (list default-directory)))

;;   (defun my-helm-do-grep-r ()
;;     (interactive)
;;     (helm-do-grep-1 (list default-directory) t)))

;; (use-package helm-swoop
;;   :ensure t
;;   :bind (("M-s o" . helm-swoop)
;;          ("M-s /" . helm-multi-swoop))
;;   :config
;;   (use-package helm-match-plugin
;;     :config
;;     (helm-match-plugin-mode 1)))

;; (use-package helm-descbinds
;;   :ensure t
;;   :bind ("C-h b" . helm-descbinds)
;;   :init
;;   (fset 'describe-bindings 'helm-descbinds)
;;   :config
;;   (require 'helm-config))

;; (use-package helm-config
;;   :if (not running-alternate-emacs)
;;   :demand t
;;   :ensure t
;;   :bind (("C-c h"   . helm-command-prefix)
;;          ("C-h a"   . helm-apropos)
;;          ("C-h e a" . my-helm-apropos)
;;          ("C-x f"   . helm-multi-files)
;;          ("M-s b"   . helm-occur)
;;          ("M-s n"   . my-helm-find)
;;          ("M-h"     . helm-resume))

;;   :preface
;;   (defun my-helm-find ()
;;     (interactive)
;;     (helm-find nil))

;;   :config
;;   (use-package helm-commands)
;;   (use-package helm-files)
;;   (use-package helm-buffers)
;;   (use-package helm-mode
;;     :diminish helm-mode
;;     :init
;;     (helm-mode 1))

;;   (use-package helm-ls-git
;;     :ensure t)

;;   (use-package helm-match-plugin
;;     :config
;;     (helm-match-plugin-mode 1))

;;   (helm-autoresize-mode 1)

;;   (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
;;   (bind-key "C-i" 'helm-execute-persistent-action helm-map)
;;   (bind-key "C-z" 'helm-select-action helm-map)
;;   (bind-key "A-v" 'helm-previous-page helm-map)

;;   (when (executable-find "curl")
;;     (setq helm-google-suggest-use-curl-p t)))

;; (use-package projectile
;;   :ensure t
;;   :diminish projectile-mode
;;   :commands projectile-global-mode
;;   :defer 5
;;   :bind-keymap ("C-c p" . projectile-command-map)
;;   :config
;;   (use-package helm-projectile
;;     :config
;;     (setq projectile-completion-system 'helm)
;;     (helm-projectile-on))
;;   (projectile-global-mode))
;; ----------------------------------

(use-package ggtags :ensure t  :defer t
  :commands ggtags-mode
  :diminish ggtags-mode)
(use-package diminish  :ensure t  :defer t)
(use-package graphene  :ensure t  :defer t)
(use-package bug-hunter  :ensure t  :defer t)
(use-package ido :ensure t
  :init
  (ido-mode t))
(use-package visual-regexp  :ensure t  :defer t)
(use-package puppet-mode  :ensure t  :defer t)
(use-package projectile  :ensure t  :defer t
  :init (progn
          (add-hook 'prog-mode-hook 'projectile-mode)))
;; (global-projectile-mode t)
(use-package switch-window  :ensure t  :defer t
  :bind ("C-x o" . switch-window))
(use-package clojure-mode  :ensure t  :defer t
  :mode      ("\\.\\(clj\\|cljs\\)$" . clojure-mode)
  :init      (defun rename-clojure-modeline ()
               (interactive)
               (setq mode-name "CLJ"))
  :config    (add-hook 'clojure-mode-hook 'rename-clojure-modeline))
(use-package cider  :ensure t  :defer t
  :init  (setq cider-words-of-inspiration '("NREPL is ready!!"))
  :config    (defalias 'cji 'cider-jack-in)
  :init      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  :diminish  (cider-mode . ""))
(use-package clojure-mode  :ensure t  :defer t)
(use-package cider  :ensure t  :defer t)
(use-package clj-refactor  :ensure t  :defer t)
(use-package paredit  :ensure t  :defer t
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(use-package rainbow-mode  :ensure t  :defer t
  :commands rainbow-mode)
(use-package buffer-move :ensure t :defer t)
(use-package ace-jump-mode  :ensure t  :defer t
  :config (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package ace-window  :ensure t  :defer t)
;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(use-package markdown-mode  :ensure t  :defer t)
(use-package yaml-mode  :ensure t  :defer t)
(use-package groovy-mode  :ensure t  :defer t)
(use-package web-mode  :ensure t  :defer t
  :mode (("\\.html$" . web-mode)
         ("\\.mustache\\'" . web-mode)))
(use-package nxml  :ensure t  :defer t
  :mode ("\\.xsl\\'" . xml-mode))
(use-package undo-tree  :ensure t  :defer t)
(use-package macrostep  :ensure t  :defer t
  :bind ("C-c e m" . macrostep-expand))
(use-package browse-kill-ring  :ensure t  :defer t)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
(use-package bm  :ensure t  :defer t)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(use-package bookmark+  :ensure t  :defer t)
(use-package browse-kill-ring+  :ensure t  :defer t)
(use-package project-persist  :ensure t  :defer t
  :config    (projectile-global-mode t))
(project-persist-mode t)
(use-package helm-descbinds  :ensure t  :defer t)

(use-package emacs-eclim  :defer t  :load-path "~/Development/repos/emacs/emacs-eclim"
  :bind ("C-c C-c" . company-complete)
  :config (progn
            (add-hook 'prog-mode-hook 'eclim-mode)
            (company-emacs-eclim-setup)))

;;(require 'eclim)
;;(global-eclim-mode)
;; (require 'eclimd)

;; regular auto-complete initialization
;;(require 'auto-complete-config)
;;(ac-config-default)

;; add the emacs-eclim source
;;(require 'ac-emacs-eclim-source)
;;(ac-emacs-eclim-config)

;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;;(define-key eclim-mode-map (kbd "C-c C-c") 'company-complete)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(use-package guide-key  :ensure t  :defer t)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

(use-package flycheck  :ensure t  :defer t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq-default tab-width 4)
(setq js-indent-level 2)
(setq jsx-indent-level 2)
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

;; Wind-move
(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-p") 'windmove-up)
(global-set-key (kbd "C-c C-n") 'windmove-right)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))


;; Function to create new functions that look for a specific pattern
(use-package cl)
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o ja")
                (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-x C-o js")
                (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-o jp")
                (ffip-create-pattern-file-finder "*.jsp"))

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

(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))


(add-hook 'after-init-hook 'global-company-mode)

(load "server")
(unless (server-running-p) (server-start))

