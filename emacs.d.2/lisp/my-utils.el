(require 'subr-x)

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
       (cd current-working-directory)
       (compile (concat "rsync-compile"))
       (end-of-buffer)))))

(defun my/insert-parentheses (arg)
  "overwrite insert-parentheses, let normal call to this command
to be `insert-parentheses', and C-u (prefix with 4) to be
`delete-pair'"
  (interactive "p")
  (if (equal arg 4)
      (delete-pair)
    (insert-parentheses)))

(defun find-deepest-ancestor-directory-containing-file (file-name &optional dir)
  ""
  (setq dir (or dir default-directory))
  (or (directory-name-p dir) (error "'dir' should be directory name"))
  (while (and (not (file-exists-p (concat dir file-name)))
              (not (equal dir "/")))
    (setq dir (file-name-as-directory (expand-file-name (concat dir "..")))))
  (if (file-exists-p (concat dir file-name)) dir nil))

(defun jgrep (regexp &optional files confirm)
  "Like \\[rgrep], except use .RsyncCompileRemotes 's directory
as search root directory"
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "my-utils.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(confirm (equal current-prefix-arg '(4))))
	   (list regexp files confirm))))))
  (let ((dir (or (find-deepest-ancestor-directory-containing-file ".RsyncCompileRemotes")
                 (find-deepest-ancestor-directory-containing-file ".git"))))
    (if (not dir)
        (error "my-utils.el: No .RsyncCompileRemotes found"))
    (rgrep regexp files dir confirm)))

(defun my/template-replace-region (pattern start end)
  (let ((begin nil)
        (deleted-line nil))
    ;; remove last "\n" of pattern if exists
    (if (equal "\n" (seq-subseq pattern -1 (length pattern)))
        (setq pattern (seq-subseq pattern 0 -1)))

    (while (> end start)
      ;; let begin,end be the region of last line
      (goto-char end)
      (forward-line -1)
      (setq begin (point))

      ;; delete last line and insert the pattern, the 'end' points to
      ;; the end of inserted pattern.
      (setq deleted-line (delete-and-extract-region begin end))
      (insert (replace-regexp-in-string "\\{\\}"
                                        (string-trim-right deleted-line)
                                        pattern))
      (insert "\n")

      (setq end begin))))

(defun my/template-replace ()
  ""
  (interactive)
  (let ((pattern (current-kill 0 t)))
    (my/template-replace-region pattern (region-beginning) (region-end))))

(provide 'my-utils)
