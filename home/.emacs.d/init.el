;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Initialise the package system.
(package-initialize)

(defvar my-packages '(graphene
		      paredit
		      rainbow-delimiters
		      projectile
		      clj-refactor
		      clojure-mode
		      clojure-test-mode
		      cider
          ahg
          emacs-eclim
          4clojure))

;; multiple-cursors and expand-region?

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'graphene)

(require 'ahg)

(require 'project-persist)
(project-persist-mode t)

(require 'rainbow-delimiters)

(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(projectile-global-mode)
(global-rainbow-delimiters-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("5a1a016301ecf6874804aef2df36ca8b957443b868049d35043a02a0c1368517" default)))
 '(desktop-save-mode t)
 '(erc-email-userid "jchochli@xpzen.com")
 '(erc-nick "jchochli")
 '(erc-nick-uniquifier "_"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq visible-bell t)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")

(load "server")
(unless (server-running-p) (server-start))


