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

(require 'evil-surround)
(global-evil-surround-mode 1)
