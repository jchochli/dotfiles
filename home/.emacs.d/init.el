;;; init.el --- my init file
;;; Commentary:
;; init file
;;; Code:
(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))


(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; (setq load-prefer-newer t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(eval-when-compile
  (require 'use-package))

;; (setq use-package-verbose t)
;; (use-package bug-hunter  :ensure t  :defer t)
;; (use-package ag :ensure t)

(use-package switch-window  :ensure t  
  :bind ("C-x o" . switch-window))

;;(use-package ace-window  :ensure t )
(use-package 4clojure :ensure t)

;; JSP
;; (use-package crappy-jsp-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.jsp$" . crappy-jsp-mode)))


(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package buffer-move :ensure t 
  :bind (("C-c b j" . buf-move-left)
         ("C-c b k" . buf-move-right)
         ("C-c b p" . buf-move-up)
         ("C-c b n" . buf-move-down)))



(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))

;;; init.el ends here
