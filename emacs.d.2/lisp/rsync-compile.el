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
    (select-frame-set-input-focus last-frame)))

(defun my/recompile ()
  "Re-compile using last *compilation* buffer (don't create a new one)"
  (interactive)
  (my/jump-to-compilation-and-do
   (lambda ()
     (recompile)
     (end-of-buffer)
     )))

(defun my/goto-first-compile-error ()
  "Jump to *compilation* window/frame, and goto the first error"
  (interactive)
  (my/jump-to-compilation-and-do
   (lambda ()
     (beginning-of-buffer)
     (compilation-next-error 1))))

(defun my/kill-compilation ()
  "Jump to *compilation* window/frame, and call (kill-compilation)"
  (interactive)
  (my/jump-to-compilation-and-do
   (lambda ()
     (kill-compilation))))

(defun jump-and-rsync-compile-target (target)
  (interactive "sTarget: ")
  (let ((current-working-directory default-directory))
    (my/jump-to-compilation-and-do
     (lambda ()
       (cd current-working-directory)
       (compile (concat "rsync-compile " (or target "")))
       (end-of-buffer)))))

(defun rsync-compile ()
  "Run 'rsync-compile' command"
  (interactive)
  (compile (concat "rsync-compile")))

(defun jump-and-rsync-compile ()
  (interactive)
  (let ((current-working-directory default-directory))
    (my/jump-to-compilation-and-do
     (lambda ()
       (ignore-errors
         (kill-compilation))
       (sleep-for 0.2)
       (cd current-working-directory)
       (compile (concat "rsync-compile"))
       (end-of-buffer)))))

(provide 'rsync-compile)
