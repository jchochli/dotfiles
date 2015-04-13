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

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)
(use-package command-log-mode  :ensure t :defer t)
;; dash
;; f
;; s
(use-package dash  :ensure t  :defer t)
(use-package f  :ensure t  :defer t)
(use-package s  :ensure t  :defer t)
(use-package diminish  :ensure t  :defer t)
(use-package bug-hunter  :ensure t  :defer t)
(use-package visual-regexp  :ensure t  :defer t)
(use-package puppet-mode  :ensure t  :defer t)

(use-package ggtags :ensure t  :defer t
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))

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

(use-package clj-refactor  :ensure t  :defer t)

(use-package paredit  :ensure t  :defer t
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package rainbow-mode  :ensure t  :defer 5
  :commands rainbow-mode)

(use-package ace-jump-mode  :ensure t  :defer 5
  :config (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package ace-window  :ensure t  :defer 5)
(use-package markdown-mode  :ensure t  :defer t)
(use-package yaml-mode  :ensure t  :defer t)
(use-package groovy-mode  :ensure t  :defer t)
(use-package undo-tree  :ensure t  :defer t)

(use-package web-mode  :ensure t  :defer t
  :mode (("\\.html$" . web-mode)
         ("\\.mustache\\'" . web-mode)))

(use-package nxml  :ensure t  :defer t
  :mode ("\\.xsl\\'" . xml-mode))

(use-package macrostep
  :defer 5
  :ensure t)

(use-package bookmark
  :ensure
  :defer 10
  :config
  (use-package bookmark+))

(use-package browse-kill-ring+
  :ensure
  :defer 10
  :commands browse-kill-ring)

(use-package project-persist  :ensure t  :defer t
  :config    (projectile-global-mode t))
(project-persist-mode t)

(use-package company  :ensure t  
  :commands company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet :ensure t)
(use-package auto-complete :ensure t)

(use-package eclimd
  :defer t
  :load-path "~/Development/repos/emacs/emacs-eclim"
  :commands start-eclimd)

(use-package emacs-eclim
  :demand t
  :load-path "~/Development/repos/emacs/emacs-eclim"
  :bind ("C-." . company-complete)
  :requires eclim
  :mode (("\\.java\\'" . eclim-mode)
	 ("\\.xsl\\'" . eclim-mode)
	 ("\\.jspx\\'" . eclim-mode))
  :config
  (use-package eclim
    :config
    (global-eclim-mode)
    (use-package company-emacs-eclim
      :requires company
      :config
      (company-emacs-eclim-setup)))
  (global-company-mode t))

(require 'eclim)
(global-eclim-mode)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(global-set-key (kbd "C-.") 'company-complete)

(use-package highlight-cl
  :ensure t
  :demand t
  :config  
  (add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
  (add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords))


(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(use-package guide-key
  :ensure t
  :defer t
  :config  
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

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

(use-package buffer-move :ensure t :defer t
  :bind (("C-c C-j" . buf-move-left)
         ("C-c C-k" . buf-move-right)
         ("C-c C-p" . buf-move-up)
         ("C-c C-n" . buf-move-down)))

(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

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

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x f"   . helm-multi-files)
         ("C-x c s" . helm-swoop)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :ensure t
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

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
           ("f" . emacs-lisp-byte-compile-and-load)
           ("j" . emacs-lisp-mode)
           ("l" . find-library)
           ("m" . macrostep-expand)
           ("r" . do-eval-region)
           ("s" . scratch)
           ("z" . byte-recompile-directory))


;; (add-hook 'ido-setup-hook
;;           (lambda ()
;;             ;; Go straight home
;;             (define-key ido-file-completion-map
;;               (kbd "~")
;;               (lambda ()
;;                 (interactive)
;;                 (if (looking-back "/")
;;                     (insert "~/")
;;                   (call-interactively 'self-insert-command))))))

(load "server")
(unless (server-running-p) (server-start))

