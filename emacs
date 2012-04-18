;;; Load up package
(require 'package)

;;; Add the original ELPA and Marmelade package repos.
(add-to-list 'package-archives
	     '("ELPA" .
	       "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("Marmalade" .
	      "http://marmalade-repo.org/packages/") t)

;;; And initialize.
(package-initialize)

;;; Refresh the packages when it's empty.
;;; Usual for fresh installs.
(when (not package-archive-contents)
  (package-refresh-contents))

;;; Define a list of packages that should be installed.
(defvar my-packages
  '(
    ido-ubiquitous
    color-theme
    color-theme-solarized
    proof-general
    )
  "A list of packages to ensure are installed at launch")

;;; Ensure the above defined packages are installed.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Load ido
(require 'ido)
(ido-mode t)
(require 'ido-ubiquitous)
(ido-ubiquitous t)
(setq ido-enable-flex-matching t)

;;; Load solarized color-theme
(require 'color-theme)
(load-theme 'solarized-light t)

;;; Load ProofGeneral
(require 'proof-site)
(setq proof-electric-terminator-enable t)
;(load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

;;; Load egg (Emacs Got Git)
(require 'egg)

;;; Disable Ctrl-z
(global-unset-key "\^z")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
