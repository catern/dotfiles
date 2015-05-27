;;;; config sources
; https://github.com/technomancy/better-defaults

;;;; bootstrap packages
(require 'package)

;; add necessary repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; start the package system
(package-initialize)

;; load the list of packages available (if necessary)
(unless package-archive-contents 
  (package-refresh-contents))

;; list the packages I want
(setq my-package-list '(evil 
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

;; stop emacs from starting the package system again after
;; finishing reading init.el
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
;; when not compiled with X, this won't be bound
(if (fboundp 'x-selection-value) 
    (setq interprogram-paste-function 'x-selection-value))

; don't ring the bell
(setq ring-bell-function 'ignore)

;;;; backups, autosaves and save places
(setq
 ;; put backups in .emacs.d, not in the directory of the original file
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; since we are putting backups in our homedir, we might need to
 ;; cross filesystems; a cross-filesystem rename is just a copy so we
 ;; might as well copy all the time
 backup-by-copying t
 ;; keep lots of versions and don't complain about it
 kept-new-versions 6
 kept-old-versions 2
 delete-old-versions t
 ;; number our backups in order
 version-control t
 auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/\\1" t))
 )
(make-directory "~/.emacs.d/autosaves/" t)

;;;; C mode
(setq comment-style 'extra-line)
(setq c-default-style 
      '((c-mode . "linux") 
	(java-mode . "java")
	(awk-mode . "awk")
	(other . "gnu")))

;;;; tramp
(require 'tramp) 
(setq tramp-default-method "ssh")

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

;;;; clojure
(evil-define-key 'normal cider-mode-map 
  "q" 'cider-popup-buffer-quit-function
  )

(evil-define-key 'normal cider-doc-mode-map 
  "q" 'cider-popup-buffer-quit-function
  ) 

(evil-define-key 'normal cider-stacktrace-mode-map 
  "q" 'cider-popup-buffer-quit-function
  ) 

(evil-define-key 'normal cider-popup-buffer-mode-map 
  "q" 'cider-popup-buffer-quit-function
  )

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
 '(safe-local-variable-values (quote ((sh-indent-comment . t))))
 '(send-mail-function (quote sendmail-send-it))
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo-tree-persist")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
