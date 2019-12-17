(defun my-utils-append-exec-path (path)
  "Append path to both the 'PATH' environment variable and
  the `exec-path' variable."
  (setenv "PATH" (concat path ":" (getenv "PATH")))
  (setq exec-path (append (list path) exec-path)))

(defun my/find-window-by-name (name)
  "Find the window by name. If found, return the window object;
otherwise, return nil"
  (let ((found-window nil))
    (walk-windows
     (lambda (window)
       (if (equal (buffer-name (window-buffer window)) name)
         (setq found-window window)))
     nil t)
    found-window))

(defun my/jump-to-compilation-and-do (compile-func)
  "call COMPILE-FUNC in the frame where *compilation* buffer is in.
The built-in `recompile' command will create a new *compilation*
window in the current frame, even if there has already a
*compilation* window in other frame."
  (let ((last-frame (selected-frame)))
    (select-window (or (my/find-window-by-name "*compilation*")
                       (selected-window)))
    (funcall compile-func)
    (end-of-buffer)
    (select-frame-set-input-focus last-frame)))

(defun my/recompile ()
  "Re-compile using last *compilation* buffer (don't create a new one)"
  (interactive)
  (my/jump-to-compilation-and-do (lambda () (recompile))))

(defun rsync-compile-target (target)
  "Run 'rsync-compile' command with given TARGET"
  (interactive "sTarget: ")
  (compile (concat "rsync-compile " (or target ""))))

(defun rsync-compile ()
  "Run 'rsync-compile' command"
  (interactive)
  (compile (concat "rsync-compile")))

(defun my/insert-parentheses (arg)
  "overwrite insert-parentheses, let normal call to this command
to be `insert-parentheses', and C-u (prefix with 4) to be
`delete-pair'"
  (interactive "p")
  (if (equal arg 4)
      (delete-pair)
    (insert-parentheses)))

(provide 'my-utils)
