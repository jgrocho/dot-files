;;; Load up package
(require 'package)

;;; Add the original ELPA and Marmelade package repos.
(add-to-list 'package-archives
	     '("ELPA" .
	       "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("Marmalade" .
	      "http://marmalade.ferrier.me.uk/packages/") t)

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
    color-theme-solarized
    ;proof-general
    auctex
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
;(ido-ubiquitous t)
(setq ido-enable-flex-matching t)

;;; Load solarized color-theme
(setq solarized-termcolors 256)
(load-theme 'solarized-light t)

;;; Load ProofGeneral
(setq proof-electric-terminator-enable t)
(load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
;(require 'proof-site)

;;; Load egg (Emacs Got Git)
;(require 'egg)

;;; Use Xe(La)TeX by default for better font support.
(setq TeX-engine 'luatex)
;;; Create PDFs by default
(setq TeX-PDF-mode t)
;;; Turn on source correlate mode, i.e. enable forward and reverse search
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-hook 'LaTeX-mode-hook 'add-my-latex-environments)
(defun add-my-latex-environments ()
  (LaTeX-add-environments
   '("dmath" LaTeX-env-label)))

;;; Define a list of modes in which to always enable flyspell-mode
(defvar my-flyspell-modes
  '(
    LaTeX-mode-hook
    )
  )

;;; Turn on flyspell-mode for all of the the above modes
(dolist (hook my-flyspell-modes)
  (add-hook hook (lambda () (flyspell-mode 1))))

;;; Define a list of modes in which to always auto-fill text
(defvar my-auto-fill-modes
  '(
    ;LaTeX-mode-hook
    )
  )

;;; Turn on auto-fill-mode for all of the above modes
(dolist (hook my-auto-fill-modes)
  (add-hook hook 'turn-on-auto-fill))

;;; Show line numbers on the side
(global-linum-mode t)

;;; Show column number in the mode line
(column-number-mode t)

;;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;;; Disable Ctrl-z
(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "dmath"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
