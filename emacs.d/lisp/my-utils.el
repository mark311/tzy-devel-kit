(require 'subr-x)

(defun my-utils-append-exec-path (path)
  "Append path to both the 'PATH' environment variable and
  the `exec-path' variable."
  (setenv "PATH" (concat path ":" (getenv "PATH")))
  (setq exec-path (append (list path) exec-path)))

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

;; 将当前选中区域（或当前行）的文本作为shell script，调用 async-shell-command执行。
(defun my/execute-shell-command ()
  (interactive)
  (if (use-region-p)
      (async-shell-command (buffer-substring (region-beginning) (region-end)))
    (async-shell-command (thing-at-point 'line t))))

(provide 'my-utils)
