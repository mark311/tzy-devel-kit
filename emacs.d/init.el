(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'super-init)

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
 '(org-agenda-files (quote ("~/Desktop/test.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "default" :family "Monaco")))))
