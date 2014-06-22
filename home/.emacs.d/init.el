;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
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
          magit
          magit-file-notify
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

;; keyboard bindings for lookup
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

(defadvice 4clojure-open-question (around 4clojure-open-question-around)
  "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
  ad-do-it
  (unless cider-current-clojure-buffer
    (cider-jack-in)))

(defun 4clojure-login (user pwd)
  "Login to 4clojure"
  (interactive "sWhat's your name? \nsAnd your password ")
  (request
   "http://www.4clojure.com/login"
   :type "POST"
   :sync t
   :headers '(
              ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:28.0) Gecko/20100101  Firefox/28.0")
              ("Referer" . "http://www.4clojure.com/login")
              )
;   :parser 'buffer-string
   :data `(("user" . ,user) ("pwd" . ,pwd))
   :success (function*
             (lambda (&key data &allow-other-keys)
               data
               )
             )
; when server send 302 header, `request` redirect request with original method POST, 
; So 4clojure will not handle this redirect and given 404
   :status-code '((404 . (lambda (&rest _) (message "login successful!"))))
   )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (graphene wombat)))
 '(custom-safe-themes
   (quote
    ("27b7d32fa83dc83ce3034e2a1fe31174c9abff70c1121e4a42b2ce08cc791aec" "5a1a016301ecf6874804aef2df36ca8b957443b868049d35043a02a0c1368517" default)))
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


