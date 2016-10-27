;;; init.el --- my init file
;;; Commentary:
;; init file
;;; Code:
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

;;(use-package f :ensure t)
;;(use-package s :ensure t)
;;(use-package dash :ensure t)
(use-package bug-hunter  :ensure t  :defer t)
(use-package visual-regexp  :ensure t  :defer t)
;;(use-package auto-complete :ensure t)
(use-package misc-cmds :ensure t)
(use-package ag :ensure t)

;; Enable puml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))

;; (use-package ido   
;;   :ensure t
;;   :init  
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   :config
;;   (progn
;;     (setq ido-case-fold t)
;;     (setq ido-everywhere t)
;;     (setq ido-enable-prefix nil)
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-create-new-buffer 'always)
;;     (setq ido-max-prospects 10)
;;     (setq ido-use-faces nil)
;;     (setq ido-file-extensions-order '(".el" ".java" ".js" ".rb"))
;;     (add-to-list 'ido-ignore-files "\\.DS_Store"))
;;     (add-hook 'ido-setup-hook
;;             (lambda ()
;;               ;; Go straight home
;;               (define-key ido-file-completion-map
;;                 (kbd "~")
;;                 (lambda ()
;;                   (interactive)
;;                   (if (looking-back "/")
;;                       (insert "~/")
;;                     (call-interactively 'self-insert-command)))))))

;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;; (use-package flx-ido
;;   :ensure t
;;   :init (flx-ido-mode 1))

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :init (ido-vertical-mode 1))

;; (use-package idomenu
;;   :ensure t
;;   :bind ("M-i" . idomenu))

;; (use-package smex
;;   :ensure t
;;   :init (smex-initialize)
;;     :bind ("M-x" . smex))

;; (use-package ido-ubiquitous
;;   :ensure t
;;   :init (ido-ubiquitous-mode 1))

;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

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

;;(use-package ace-window  :ensure t )
(use-package undo-tree  :ensure t  )
(use-package 4clojure :ensure t)

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

;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))

;; (use-package company
;;   :ensure t
;;   :bind ("M-/" . company-complete)
;;   :init
;;   (global-company-mode 1)
;;   :config
;;   (bind-keys :map company-active-map
;;              ("C-n" . company-select-next)
;;              ("C-p" . company-select-previous)
;;              ("C-d" . company-show-doc-buffer)
;;              ("<tab>" . company-complete)))

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

(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)


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

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

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

;; (ensure-packages-compiled)

;; (load "server")
;; (unless (server-running-p)
;;   (server-start))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))

;;; init.el ends here
