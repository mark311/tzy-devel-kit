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
    (end-of-buffer)
    (select-frame-set-input-focus last-frame)))

(defun my/insert-parentheses (arg)
  "overwrite insert-parentheses, let normal call to this command
to be `insert-parentheses', and C-u (prefix with 4) to be
`delete-pair'"
  (interactive "p")
  (if (equal arg 4)
      (delete-pair)
    (insert-parentheses)))

(provide 'my-utils)
