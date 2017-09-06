(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'super-init)

;; Add binary search path
(my-utils-append-exec-path "/usr/local/bin/")

;; Set plantuml.jar location
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.14/libexec/plantuml.jar")
