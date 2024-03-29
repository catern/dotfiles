(setq package-selected-packages
      '(magit cyberpunk-theme csv-mode))

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
;; don't ring the bell
(setq ring-bell-function 'ignore)

;;;; ffap
(global-set-key [remap find-file] 'find-file-at-point)
(setq ffap-require-prefix t)

;;;; miscellaneous
; don't cry about visiting my version controlled dotfiles
(setq vc-follow-symlinks nil)
;; there are many bindings that I like to use, which use minibuffers;
;; so let's make it possible to use those bindings when already in a minibuffer
(setq enable-recursive-minibuffers t)
;; this is pointlessly small by default
(setq yank-menu-length 250)

;; jump to the first error we see when we compile.
;; otherwise we'd have to watch and wait until it appears before we hit M-g M-n
;; (hmm, maybe M-g M-n should be enhanced to automatically have this behavior,
;;  when the error buffer is empty?)
(setq compilation-scroll-output 'first-error)

;;;; gud
(setq gdb-display-io-nopopup t)

;;;; org-mode
;; org-mode doesn't believe in defaults, which is annoying
(setq
 org-agenda-files '("~/org/")
 org-archive-subtree-save-file-p nil
 org-log-done 'time
 org-html-prefer-user-labels t
 org-enforce-todo-dependencies t)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;;; dired
(setq dired-dwim-target t)

;;;; lifelog
(load-file "~/.emacs.d/lifelog.el")

;;;; windmove
(windmove-default-keybindings)

;;;; tab-bar-mode
(setq tab-bar-select-tab-modifiers '(control)
      tab-bar-show 1)

;;;; mail
(setq gnus-select-method '(nntp "news.gmane.io"))

;;;; magit
(require 'magit)
(require 'magit-extras)
;; this is expensive to compute
(remove-hook 'magit-status-headers-hook #'magit-insert-tags-header)

;;;; dired
;; omit dotfiles
(setq dired-omit-files (rx line-start "." (not (any ".")) (zero-or-more anything) line-end))
(add-hook 'dired-mode-hook #'dired-omit-mode)

;;;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")

;;;; nice little customization for presentations
(require 'page-ext)
;; TODO this should just be a setting for pages-{next,previous}-page,
;; so they don't narrow if not already narrowed.
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
;; or better yet, a custom org-capture?
(defun jrnl ()
  "Open my journal"
  (interactive)
  (find-file (format-time-string "~/Documents/journal/%Y-%m-%d.org")))

;; environment variables for subprocesses; we want these only in Emacs
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; stateful usage of customize plus local and private settings go here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
