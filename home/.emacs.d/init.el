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


(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))

;;; init.el ends here
