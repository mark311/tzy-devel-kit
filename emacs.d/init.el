;; Common
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "/usr/local/share/gtags")

(ido-mode)
;; (desktop-save-mode 1)
(setq completion-ignore-case nil)

(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))

(defun move-end-of-line-and-newline-and-indent()
  "At anywhere in a line, goto end and indent-return"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
)
(global-set-key (kbd "C-M-j") 'move-end-of-line-and-newline-and-indent)

(defun move-beginning-of-line-and-open-line-and-indent()
  "At anywhere in a line, insert an empty line before it and indent"
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-for-tab-command nil)
)
(global-set-key (kbd "C-o") 'move-beginning-of-line-and-open-line-and-indent)

;; Don't show welcome buffer
(setq inhibit-startup-message t)

;; Gtags/Global
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
   '(lambda ()
      (gtags-mode 1)
))
(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)
))
(setq gtags-suggested-key-mapping t)
(setq gtags-auto-update t)

;; Coding Style
(setq c-default-style "linux"
          c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Highlight
(require 'highlight-symbol)
(global-set-key (kbd "<f8>") 'highlight-symbol-at-point)

;; Switch window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x C-o") 'switch-window)

;; Zoom window
(require 'zoom-window)
(global-set-key (kbd "C-x 9") 'zoom-window-zoom)
(setq zoom-window-mode-line-color "navy blue")

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-?") 'yas-expand)

;; Custom Set Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
