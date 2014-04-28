;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Initialise the package system.
(package-initialize)

(unless (package-installed-p 'graphene)
  (package-refresh-contents)
  (package-install 'graphene))

(require 'graphene)

(require 'project-persist)
(project-persist-mode t)

(unless (package-installed-p 'cider)
  (package-install 'cider))

(unless (package-installed-p 'paredit)
  (package-install 'paredit))
(require 'paredit)

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-rainbow-delimiters-mode)

;; paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-M-<up>") 'cider-repl-backward-input))

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-M-<down>") 'cider-repl-forward-input))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("5a1a016301ecf6874804aef2df36ca8b957443b868049d35043a02a0c1368517" default)))
 '(desktop-save-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq visible-bell t)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
