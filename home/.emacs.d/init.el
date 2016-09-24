;;;; config sources
; https://github.com/technomancy/better-defaults

;;;; bootstrap packages
;; start the package system
(package-initialize)

;; add necessary repos
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; download repository metadata
(unless package-archive-contents
  (package-refresh-contents))

;; list the packages I want
(setq my-package-list '(evil 
			evil-surround
			cyberpunk-theme 
			magit 
			better-defaults 
			auctex 
			;; for org HTML export
			htmlize
                        ))

;; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Don't run package-initialize again after finishing reading init.el
;; Just a small startup time optimization
(setq package-enable-at-startup nil)

;;;; visuals
;; enable cyberpunk theme yeah!
(load-theme 'cyberpunk t)
;; enable syntax highlighting
(global-font-lock-mode t)
;; scroll incrementally, not screenfulls at a time
(setq scroll-conservatively 1000)

;;;; miscellaneous
; don't cry about visiting my version controlled dotfiles
(setq vc-follow-symlinks nil)
;; be willing to insert tabs; overrides the better-defaults setting
(setq-default indent-tabs-mode t)

; don't ring the bell
(setq ring-bell-function 'ignore)

;;;; backup files are automatically created on save
(setq
 ;; number our backups in order
 version-control t
 ;; backup by copying is slower but safer
 backup-by-copying t
 ;; silently delete excess backups
 delete-old-versions t
 )

;;;; C mode
(setq comment-style 'extra-line)
(setq c-default-style 
      '((c-mode . "linux") 
	(java-mode . "java")
	(awk-mode . "awk")
	(other . "gnu")))

;;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-persist")))

;;;; evil
(load-file "~/.emacs.d/evil.el")

;;;; org-mode configuration
(require 'org)

; do syntax highlighting in #+begin_src blocks
(setq org-src-fontify-natively t)

;;;; dired
(setq dired-dwim-target t)

;;;; lifelog
(load-file "~/.emacs.d/lifelog.el")

;; local.el
(setq my-local-config-file "~/.emacs.d/local.el")
(if (file-exists-p my-local-config-file)
    (load-file my-local-config-file))

(setq gnus-select-method '(nntp "news.gmane.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote bully))
 '(custom-safe-themes (quote ("7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "dc6c0b236bb09603babadd87329aa857e286ee36715811519d4bfe6278ee4367" default)))
 '(org-enforce-todo-dependencies t)
 '(safe-local-variable-values (quote ((sh-indent-comment . t))))
 '(send-mail-function (quote sendmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
