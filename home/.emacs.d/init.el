;;; init.el --- init file
;; Require Emacs' package functionality
(setq debug-on-error t)
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
          buffer-move
          ace-jump-mode
          ace-window
          undo-tree
		      clj-refactor
		      clojure-mode
		      clojure-test-mode
		      cider
          company
          company-cider
		      magit
		      magit-filenotify
		      ahg
		      emacs-eclim     
		      4clojure))

;; multiple-cursors and expand-region?

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun ad-advised-definition-p (definition)
  "Return non-nil if DEFINITION was generated from advice information."
  (if (or (ad-lambda-p definition)
	  (macrop definition)
	  (ad-compiled-p definition))
      (let ((docstring (ad-docstring definition)))
	(and (stringp docstring)
	     (get-text-property 0 'dynamic-docstring-function docstring)))))

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

(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(custom-enabled-themes (quote (graphene wombat)))
 '(custom-safe-themes
   (quote
    ("f1ea873350bbb910a551854d700dfa7a16f0b6e7b9e88e12e012d9f0f881d083" "27b7d32fa83dc83ce3034e2a1fe31174c9abff70c1121e4a42b2ce08cc791aec" "5a1a016301ecf6874804aef2df36ca8b957443b868049d35043a02a0c1368517" default)))
 '(desktop-save-mode t)
 '(eclim-eclipse-dirs (quote ("~/Development/bin/eclipse-luna")))
 '(eclim-executable "~/Development/bin/eclipse-luna/eclim")
 '(erc-email-userid "jchochli@xpzen.com")
 '(erc-nick "jchochli")
 '(erc-nick-uniquifier "_"))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(setq visible-bell t)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(define-key eclim-mode-map (kbd "C-c C-SPC") 'company-complete)

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

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


(defadvice 4clojure-open-question (around 4clojure-open-question-around)
  "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
  ad-do-it
  (unless cider-current-clojure-buffer
    (cider-jack-in)))

;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

;; (add-hook 'ido-setup-hook
;;  (lambda ()
;;    ;; Go straight home
;;    (define-key ido-file-completion-map
;;      (kbd "~")
;;      (lambda ()
;;        (interactive)
;;        (if (looking-back "/")
;;            (insert "~/")
;;          (call-interactively 'self-insert-command))))))


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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "server")
(unless (server-running-p) (server-start))


