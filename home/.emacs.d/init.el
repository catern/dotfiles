;;;; config sources
; https://github.com/technomancy/better-defaults

;;;; bootstrap packages
;; start the package system
(package-initialize)

;; add necessary repos
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")
	))

;; download repository metadata
(unless package-archive-contents
  (package-refresh-contents))

;; list the packages I want
(setq package-selected-packages '(
        undo-tree
        cyberpunk-theme
        magit
        better-defaults
        auctex
        ;; for org HTML export
        htmlize
        circe
        elpy
))

;; install the missing packages
(package-install-selected-packages)

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
;; find-file-at-point is very useful
(global-set-key (kbd "C-c f") 'find-file-at-point)

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
(setq compilation-scroll-output 'first-error)

;;;; gud
(require 'gdb-mi)
(setq gdb-display-io-nopopup t)

;;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-persist")))

;;;; org-mode configuration
(require 'org)
(setq org-agenda-files '("~/org/"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

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

;;;; circe
(require 'circe)
(setq
 ;; don't show joins/parts until the user in question speaks
 circe-reduce-lurker-spam t
 ;; show topic changes as a diff
 circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
;; prompt to send large pastes to a pastebin
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
;; set circe-network-options
(load-file "~/.emacs.d/private.el")

;;;; tracking
(require 'tracking)
;; ignore any circe channel buffer, except if I'm highlighted
(setq tracking-ignored-buffers '(("#" circe-highlight-nick-face)))

;;;; lui
;; use native emacs word wrapping/reflowing
(setq lui-time-stamp-position 'right-margin
      lui-fill-type nil)
(defun my-lui-setup ()
  (setq fringes-outside-margins t
	right-margin-width 8
	word-wrap t
	wrap-prefix "    "))
(add-hook 'lui-mode-hook 'my-lui-setup)

(setq gnus-select-method '(nntp "news.gmane.org"))

;;;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;; elpy
(elpy-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
