;;;; config sources
;https://github.com/technomancy/better-defaults
; list the packages you want
(setq my-package-list '(evil cyberpunk-theme))

(mkdir "~/.emacs.d/plugins/" t)
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;; visuals
(load-theme 'cyberpunk t)
; enable syntax highlighting
(global-font-lock-mode t)
(transient-mark-mode 1)
; scroll incrementally, not screenfulls at a time
(setq scroll-step 1)
; menubar, toolbar, scrollbar off
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
; show matching parens
(show-paren-mode 1)

;;;; miscellaneous
; don't cry about visiting my version controlled dotfiles
(setq vc-follow-symlinks t)
; better completions
(ido-mode t)
(setq ido-enable-flex-matching t)
; if visiting two different files with the same name,
; label the buffer with the full path
; instead of name<1> name<2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
; save position in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-positions")) 
; spaces instead of tabs
(setq-default indent-tabs-mode nil)
; use X11 clipboard when in graphical mode
(setq x-select-enable-clipboard t
      ;x-select-enable-primary t
      ; put existing X selection in kill ring
      save-interprogram-paste-before-kill t
      ) 
(setq interprogram-paste-function 'x-selection-value)

; i don't understand what this does
(setq mouse-yank-at-point t)
; include more things in apropos searches
(setq apropos-do-all t)
;; some better default bindings
; better expansion
(global-set-key (kbd "M-/") 'hippie-expand)
; better buffer selection
(global-set-key (kbd "C-x C-b") 'ibuffer)
; regex-aware searching
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
; don't ring the bell
(setq ring-bell-function 'ignore)


;;;; backups, autosaves and save places
(setq backup-by-copying t ; don't clobber symlinks
      auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))) ; backup save location
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ; use versioned backups
(make-directory "~/.emacs.d/autosaves/" t)

;;;; tramp
(require 'tramp) 
(setq tramp-default-method "ssh")

;;;; latex
(setq TeX-PDF-mode t)

;;;; evil configuration
(require 'undo-tree)
(require 'evil)
(evil-mode 1)

; be a little more lenient to tolerate key delay over ssh
(setq evil-esc-delay .05)

(setq evil-default-cursor t)
(set-cursor-color "white")

; usable esc
(defun minibuffer-keyboard-quit () 
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))(abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-cancel)
(global-set-key [escape] 'keyboard-escape-quit)

;;;; org-mode configuration
(require 'org)

; make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
; do syntax highlighting in #+begin_src blocks
(setq org-src-fontify-natively t)


;; evil and org mode
(evil-define-key 'normal org-mode-map 
  "go" 'org-open-at-point
  (kbd "TAB") 'org-cycle
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  "-" 'org-cycle-list-bullet
  )

(define-key evil-normal-state-map "gt" 'org-agenda)

(mapc (lambda (state)
        (evil-define-key state org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown))
      '(normal insert))


;;;; life-logging
(setq my-buffer-activity-logfile "~/.emacs.d/bufferlog")

(defun my-store-name-of-active-buffer ()
  "Stores the name of the active buffer in my-buffer-activity-logfile"
  (interactive)
  (write-region 
   (concat (format-time-string "%Y-%m-%d %H:%M:%S") 
           " "
           (or (buffer-file-name (current-buffer))
               (buffer-name (current-buffer))
               )
           "\n") 
   nil my-buffer-activity-logfile 'append 'silent)
  )
  
(defun my-store-lifelog-data ()
  "Activates all my lifelogging functions"
  (interactive)
  (if 
      (time-less-p 
       (or 
        (current-idle-time) 
        (seconds-to-time 0))
       (seconds-to-time 5) 
       )
  (my-store-name-of-active-buffer))
)

(setq lifelog-timer (run-with-timer 1 5 'my-store-lifelog-data))


;; local.el
(setq my-local-config-file  "~/.emacs.d/local.el")

(unless (file-exists-p my-local-config-file)
  (write-region "" nil my-local-config-file)) 

(load-file my-local-config-file)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote bully))
 '(custom-safe-themes (quote ("7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "dc6c0b236bb09603babadd87329aa857e286ee36715811519d4bfe6278ee4367" default)))
 '(org-enforce-todo-dependencies t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo-tree-persist")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
