;;; init.el --- init file
;; Require Emacs' package functionality
(setq debug-on-error t)
(require 'package)

;; Add the Melpa repository to the list of package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "http://melpa.org/packages/")))

(add-to-list 'exec-path "/usr/local/bin")

;; Initialise the package system.
(package-initialize)

(defvar my-packages
  '(graphene
    paredit
    rainbow-delimiters
    projectile
    buffer-move
    ace-jump-mode
    ace-window
    undo-tree
    clojure-mode
    cider
    clj-refactor    
    company
    magit
    magit-filenotify
    ahg
    browse-kill-ring
    emacs-eclim
    know-your-http-well
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

;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(projectile-global-mode)
(global-rainbow-delimiters-mode)

(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(custom-enabled-themes (quote (graphene wombat)))
 '(custom-safe-themes
   (quote
    ("cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "f1ea873350bbb910a551854d700dfa7a16f0b6e7b9e88e12e012d9f0f881d083" "27b7d32fa83dc83ce3034e2a1fe31174c9abff70c1121e4a42b2ce08cc791aec" "5a1a016301ecf6874804aef2df36ca8b957443b868049d35043a02a0c1368517" default)))
 '(desktop-save-mode t)
 '(eclim-eclipse-dirs (quote ("~/Development/bin/eclipse-luna")))
 '(eclim-executable "~/Development/bin/eclipse-luna/eclim")
 '(erc-email-userid "jchochli@xpzen.com")
 '(erc-nick "jchochli")
 '(erc-nick-uniquifier "_")
 '(indent-tabs-mode nil)
 '(tab-width 4))

(setq javascript-indent-level 4)
(setq c-basic-indent 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(defun my-sml-mode-hook () "Local defaults for SML mode"
       (setq indent-tabs-mode nil))     ; never ever indent with tabs
(add-hook 'sml-mode-hook 'my-sml-mode-hook)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(setq visible-bell t)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
(setq tramp-default-method "ssh")

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



(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xsl\\'" . web-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Function to create new functions that look for a specific pattern
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

(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)

(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
       (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))

;; (setq ansi-term-color-vector [unspecified "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])

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


;; To load a file and run sml repl -> Press C-c C-v inside a sml file.
;; To run the current files tests -> Press C-c C-r inside a sml file. It assumes the tests are inside the same directory.
;; For example, If your code file is hw1.sml your test file should be named as hw1tests.sml in the same directory
(require 'sml-mode)

;;; Credits: https://gist.github.com/koddo/4555655
(defun my-sml-restart-repl-and-load-current-file ()
  (interactive)
  (save-buffer)
  (ignore-errors (with-current-buffer "*sml*"
                   (comint-interrupt-subjob)
                   (comint-send-eof)
                   (let ((some-time 0.1))
                     (while (process-status (get-process "sml"))
                       (sleep-for some-time)))))
  (flet ((sml--read-run-cmd ()
           '("sml" "" nil))) ; (command args host)
    (sml-prog-proc-send-buffer t)))
 
(defun my-sml-restart-repl-and-load-current-file-tests ()
  (interactive)
  (save-buffer)
  (let ((code-file-name (replace-regexp-in-string "tests.sml$" ".sml" (buffer-file-name))))
    (let ((test-file-name (replace-regexp-in-string ".sml$" "tests.sml" code-file-name))
          (code-buffer (buffer-name)))
      (find-file test-file-name)
      (my-sml-restart-repl-and-load-current-file)
      (switch-to-buffer-other-window code-buffer))))
 
(eval-after-load 'sml-mode
  '(progn
    (define-key sml-mode-map (kbd "C-j") 'reindent-then-newline-and-indent)
    (define-key sml-mode-map (kbd "C-c C-s") 'sml-run)
    (define-key sml-mode-map (kbd "C-c C-v") 'my-sml-restart-repl-and-load-current-file)
    (define-key sml-mode-map (kbd "C-c C-r") 'my-sml-restart-repl-and-load-current-file-tests)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "server")
(unless (server-running-p) (server-start))


