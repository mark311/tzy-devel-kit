(require 'my-utils)
(require 'json)

(defvar rsync-compile-profile-history '())


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


(defun rsync-compile--load-config ()
  (let* ((rsynccompile-filename ".rsynccompile")
         (project-root-directory (or (find-deepest-ancestor-directory-containing-file rsynccompile-filename)
                                     (error "can't find .rsynccompile in any ancestor directories of %s" default-directory))))
    (condition-case err
        (let ((json-key-type 'string))
          (json-read-file (concat project-root-directory "/" rsynccompile-filename)))
      (error (error "parse .rsynccompile config file failed: %s" (error-message-string err))))))

(defun rsync-compile--list-profiles (rsync-compile-config)
  (mapcar 'car (cdr (assoc "profiles" rsync-compile-config))))

(defun rsync-compile--get-profile (rsync-compile-config profile-name)
  (cdr (assoc profile-name (cdr (assoc "profiles" rsync-compile-config)))))

(defun rsync-compile--run-profile (profile-name)
  (let* ((rsync-compile-config (rsync-compile--load-config))
         (profile (rsync-compile--get-profile rsync-compile-config profile-name))
         (server   (cdr (assoc "server" rsync-compile-config)))
         (rpath    (cdr (assoc "path" rsync-compile-config)))
         (lpath    (find-deepest-ancestor-directory-containing-file ".rsynccompile")) ; TODO: 考虑与 rsync-compile--load-config中的调用复用。
         (excludes (cdr (assoc "exclude" rsync-compile-config)))
         (command  (cdr (assoc "cmd" profile)))
         (rsynccompile-command-line (format "rsync-compile --v2 %s %s %s %s %s"
                                            (prin1-to-string server)
                                            (prin1-to-string rpath)
                                            (prin1-to-string lpath)
                                            (prin1-to-string excludes)
                                            (prin1-to-string command))))
    (compile rsynccompile-command-line)
    ;; the above `compile' call may cause another frame (perhaps
    ;; containing a *compilation* buffer) to be activated BUT not be
    ;; selected (for the difference between "activated" and
    ;; "selected", see the help of the function
    ;; `select-frame-set-input-focus'), we call the following line to
    ;; set the input focus back to the current selected frame.
    (select-frame-set-input-focus (selected-frame))))

(defun rsync-compile-prompt ()
  (interactive)
  (let ((profile-name (completing-read "Choose a profile to run: "
                                       (rsync-compile--list-profiles (rsync-compile--load-config))
                                       nil t nil
                                       '(rsync-compile-profile-history . 1))))
    (rsync-compile--run-profile profile-name)))

(defun rsync-compile-last ()
  (interactive)
  (let ((profile-name (car rsync-compile-profile-history)))
    (rsync-compile--run-profile profile-name)))

(provide 'rsync-compile)
