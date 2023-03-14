;;;; config sources
; https://github.com/technomancy/better-defaults

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-selected-packages
      '(slime-volleyball circe
	notmuch mentor htmlize graphviz-dot-mode
	ggtags envrc cyberpunk-theme csv-mode auctex
	nix-mode))

(package-install-selected-packages)

;;;; Configurations stolen from better-defaults
(tool-bar-mode -1)
(scroll-bar-mode -1)

(save-place-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(savehist-mode 1)
(setq
 uniquify-buffer-name-style 'forward
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t
 load-prefer-newer t
 ediff-window-setup-function 'ediff-setup-windows-plain)

 ;; backup by copying is slower but safer
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;;; visuals
;; enable cyberpunk theme yeah!
(load-theme 'cyberpunk t)
;; scroll incrementally, not screenfulls at a time
(setq scroll-conservatively 1000)

;;;; ffap
(ffap-bindings)
(setq ffap-require-prefix t)

;;;; miscellaneous
; don't cry about visiting my version controlled dotfiles
(setq vc-follow-symlinks nil)
;; there are many bindings that I like to use, which use minibuffers;
;; so let's make it possible to use those bindings when already in a minibuffer
(setq enable-recursive-minibuffers t)

; don't ring the bell
(setq ring-bell-function 'ignore)

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

;;;; org-mode configuration
(require 'org)
(setq org-agenda-files '("~/org/"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;;;; dired
(setq dired-dwim-target t)

;;;; lifelog
(load-file "~/.emacs.d/lifelog.el")


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

;;;; tracking
(require 'tracking)
;; ignore any circe channel buffer, except if I'm highlighted
(setq tracking-ignored-buffers '(("#" circe-highlight-nick-face)))

;;;; windmove
(windmove-default-keybindings)

;;;; tab-bar-mode
(require 'tab-bar)
(setq tab-bar-select-tab-modifiers '(control)
      tab-bar-show 1)
(tab-bar-mode)

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

(setq gnus-select-method '(nntp "news.gmane.io"))

;;;; magit
(require 'magit)
(require 'project)
(require 'magit-extras)

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

;;;; nice little customization for presentations
(require 'page-ext)
(defun my-backward-page (&optional count)
  "Like backward-page, but moves page even if narrowed, re-narrowing appropriately"
  (interactive)
  (if (buffer-narrowed-p)
      (pages-previous-page count)
    (backward-page count)))

(defun my-forward-page (&optional count)
  "Like forward-page, but moves page even if narrowed, re-narrowing appropriately"
  (interactive)
  (if (buffer-narrowed-p)
      (pages-next-page count)
    (forward-page)))

(define-key ctl-x-map "[" 'my-backward-page)
(define-key ctl-x-map "]" 'my-forward-page)

;; TODO should try using diary for this?
(defun jrnl ()
  "Open my journal"
  (interactive)
  (find-file (format-time-string "~/Documents/journal/%Y-%m-%d.org")))

;; environment variables for subprocesses; we want these only in Emacs
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

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
;; stateful usage of customize plus local and private settings go here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
