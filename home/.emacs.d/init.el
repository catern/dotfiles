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

;;;; cscope
(cscope-setup)
(setq cscope-option-do-not-update-database t)

;;;; evil configuration
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-persist")))
(require 'evil)

;; Default to emacs-state and only use normal-state in modes where
;; we'll be doing a lot of text editing.
(setq evil-default-state 'emacs
      evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil
      evil-normal-state-modes '(text-mode prog-mode fundamental-mode
					  css-mode conf-mode
					  TeX-mode LaTeX-mode
					  org-mode
					  diff-mode))

;; Furthermore, we'll override evil-initial state to also put us in
;; normal-state in modes derived from those text editing modes, which
;; should cover just about everything we want it to cover.
;; First we need some helpers:
(defun my-real-function (fun)
  "Figure out the actual symbol behind a function.
Returns a different symbol if FUN is an alias, otherwise FUN."
  (let ((symbol-function (symbol-function fun)))
    (if (symbolp symbol-function)
	symbol-function
      fun)))

(defun my-derived-mode-p (mode modes)
  "Non-nil if the current major mode is derived from one of MODES."
  (let ((parent (my-real-function mode)))
    (while (and parent (not (memq parent modes)))
      (setq parent (my-real-function (get parent 'derived-mode-parent))))
    parent))

;; Override here:
(defun evil-initial-state (mode &optional default)
  "Return the Evil state to use for MODE.
Returns DEFAULT if no initial state is associated with MODE.
The initial state for a mode can be set with
`evil-set-initial-state'."
  (let (state modes)
    (catch 'done
      (dolist (entry (nreverse (evil-state-property t :modes)) default)
	(setq state (car entry)
	      modes (symbol-value (cdr entry)))
	(when (or (memq mode modes)
		  (my-derived-mode-p mode modes))
	  (throw 'done state)))))))

(evil-mode 1)

;; unmap everything but Esc from evil-insert-state-map
;; This means insert-state will be like vanilla Emacs, except for Esc
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map [escape] 'evil-normal-state)
;; selectively unmap some things from evil-normal-state-map
;; The vim bindings for these characters conflict with Emacs bindings
(define-key evil-normal-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map (kbd "C-e") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "C-i") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)

; be a little more lenient to tolerate key delay over ssh
(setq evil-esc-delay .05)

;;;; org-mode configuration
(require 'org)

; do syntax highlighting in #+begin_src blocks
(setq org-src-fontify-natively t)

;; evil and org mode
(evil-define-key 'normal org-mode-map 
  (kbd "TAB") 'org-cycle
  )

;;;; life-logging
(setq my-buffer-activity-logfile (expand-file-name "~/.emacs.d/bufferlog"))

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

(setq lifelog-timer (when (daemonp)
		      (run-with-timer 1 5 'my-store-lifelog-data)))


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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
