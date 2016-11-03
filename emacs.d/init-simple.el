(ido-mode)
(setq completion-ignore-case nil)
(setq column-number-mode t)

(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))

;; Remap help-map with M-h instead of C-h, make C-h to be translated to DEL.
;; Note that C-M-h will be translated to M-DEL correctly under terminals, but
;; incorrectly under GUI version emacs. So bind it explicitly.
;; See: <https://www.emacswiki.org/emacs/BackspaceKey>
(global-set-key (kbd "M-h") help-map)
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-M-h") 'backward-kill-word)  ;; for compatibility of GUI version

;; Don't show welcome buffer
(setq inhibit-startup-message t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Linum mode line number format
(setq linum-format "%4d  ")
(global-linum-mode)

;; Coding Style
(setq c-default-style "linux"
          c-basic-offset 4)
(setq-default indent-tabs-mode nil)
