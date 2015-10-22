;; Common
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/usr/local/share/gtags")

(ido-mode)
;; (desktop-save-mode 1)
(setq completion-ignore-case nil)
(setq column-number-mode t)

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

;; TODO still cycling, need go back to last
(defun goto-last-window()
  "At anywhere in a line, goto end and indent-return"
  (interactive)
  (other-window -1))

;; Cycle to the opposite when reach to the end
(defun windmove-up-cycle()
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down)))))

(defun windmove-down-cycle()
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up)))))

(defun windmove-right-cycle()
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left)))))

(defun windmove-left-cycle()
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right)))))


;; Don't show welcome buffer
(setq inhibit-startup-message t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Linum mode line number format
(setq linum-format "%4d  ")
(require 'linum-off)
(global-linum-mode)

;; Helm
(add-to-list 'load-path "~/.emacs.d/3rdlib/helm")
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "M-/") 'helm-dabbrev)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "M-s f") 'helm-occur)
(global-set-key (kbd "M-s M-f") 'helm-occur)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(helm-mode 1)

;; Enable helm-gtags-mode
(autoload 'helm-gtags-mode "helm-gtags" "" t)
(add-hook 'c-mode-common-hook 'helm-gtags-mode)
(setq helm-gtags-auto-update t)

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "C-]") 'helm-gtags-find-tag-from-here)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "C-c s r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "C-c s s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "C-c s g") 'helm-gtags-find-pattern)
     (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     )
  )

;; JDEE
(add-to-list 'load-path "~/.emacs.d/3rdlib/jdee-2.4.1/lisp")
(load "jde")

;; Coding Style
(setq c-default-style "linux"
          c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Highlight
(require 'highlight-symbol)
(global-set-key (kbd "M-&") 'highlight-symbol-query-replace)
(global-set-key (kbd "M-*") 'highlight-symbol-at-point)

;; Switch window
(require 'switch-window)
(global-set-key (kbd "M-g y") 'switch-window)
(global-set-key (kbd "M-g i") 'goto-last-window)
(global-set-key (kbd "M-g h") 'windmove-left-cycle)
(global-set-key (kbd "M-g j") 'windmove-down-cycle)
(global-set-key (kbd "M-g k") 'windmove-up-cycle)
(global-set-key (kbd "M-g l") 'windmove-right-cycle)
(global-set-key (kbd "M-g M-h") 'windmove-left-cycle)
(global-set-key (kbd "M-g M-j") 'windmove-down-cycle)
(global-set-key (kbd "M-g M-k") 'windmove-up-cycle)
(global-set-key (kbd "M-g M-l") 'windmove-right-cycle)

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

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-width 1)
;;(setq fci-rule-color "brightblack")
;;(add-hook 'prog-mode-hook 'fci-mode)

;; This snippet enables lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(setq lua-indent-level 4)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Settings that only for GUI mode
(if (display-graphic-p)
    (progn
      (custom-set-faces
       ;; set background color in GUI mode, changing from white to black.
       '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "default" :family "Monaco")))))
      ;; set Chinese font, or the when showing Italic Chinese characters, only rectangle block shown
      (set-fontset-font (frame-parameter nil 'font)
                        'han
                        (font-spec :family "Hiragino Sans GB" ))))

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
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-refine-added ((t (:inherit diff-refine-change))))
 '(diff-refine-change ((t (:weight bold))))
 '(diff-refine-removed ((t (:inherit diff-refine-change))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(font-lock-builtin-face ((t (:foreground "white"))))
 '(helm-buffer-directory ((t (:background "black" :foreground "#5c5cff"))))
 '(helm-ff-directory ((t (:background "black" :foreground "#5c5cff"))))
 '(helm-selection ((t (:background "#3a3a3a" :underline t))))
 '(org-table ((t (:foreground "green"))))
 '(region ((t (:background "#00008e")))))
