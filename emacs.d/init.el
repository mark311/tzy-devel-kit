(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-frozen)

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))

