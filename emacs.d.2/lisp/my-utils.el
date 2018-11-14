(defun my-utils-append-exec-path (path)
  "Append path to both the 'PATH' environment variable and
  the `exec-path' variable."
  (setenv "PATH" (concat path ":" (getenv "PATH")))
  (setq exec-path (append (list path) exec-path)))

(defun my/recompile ()
  "Recompile in the frame where *compilation* buffer is in.
The built-in `recompile' command will create a new *compilation*
window in the current frame, even if there has already a
*compilation* window in other frame."
  (interactive)
  (let ((last-frame (selected-frame)))
  (select-frame-by-name "*compilation*")
  (recompile)
  (select-frame last-frame)))


(provide 'my-utils)
