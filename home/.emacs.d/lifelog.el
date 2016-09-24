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
