;;;; config sources
; https://github.com/technomancy/better-defaults

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Enable better-defaults
(require 'better-defaults)

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
;; there are many bindings that I like to use, which use minibuffers;
;; so let's make it possible to use those bindings when already in a minibuffer
(setq enable-recursive-minibuffers t)

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

;;;; org-mode configuration
(require 'org)
(setq org-agenda-files '("~/org/"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

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

;;;; dired
(require 'dired-x)
;; make the default omit, omit dotfiles
(setq dired-omit-files (rx line-start "." (not (any ".")) (zero-or-more anything) line-end))

;;;; ggtags
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (ggtags-mode 1))))

;;;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")

;;;; shell hack
(require 'cl)
(require 'project)
(defun my-project-shell ()
  (interactive)
  (let ((default-directory (first (project-roots (project-current 'prompt)))))
    (shell (concat "*shell-" (file-name-nondirectory (directory-file-name default-directory))))))
(global-set-key (kbd "C-c i") 'my-project-shell)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "dc6c0b236bb09603babadd87329aa857e286ee36715811519d4bfe6278ee4367" default)))
 '(org-enforce-todo-dependencies t)
 '(safe-local-variable-values (quote ((sh-indent-comment . t))))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
