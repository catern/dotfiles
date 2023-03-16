(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(dolist (package '(circe notmuch mentor htmlize nix-mode auctex ggtags envrc))
  (add-to-list 'package-selected-packages package))

(package-install-selected-packages)

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
(require 'lui-logging)
(enable-lui-logging-globally)
(setq lui-logging-file-format "{buffer}/%Y-%m-%d.txt")

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

