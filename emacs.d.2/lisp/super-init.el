(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; --------------------------------------------------------------------
;; Require & Load Libraries
;; --------------------------------------------------------------------
(require 'helm-config)  ;; package helm
(require 'linum-off)    ;; package linum-off
(require 'yasnippet)    ;; package yasnippet
(autoload 'helm-gtags-mode "helm-gtags" "" t)  ;; package helm-gtags
(add-hook 'c-mode-common-hook 'helm-gtags-mode)

;; --------------------------------------------------------------------
;; Personal Scripts
;; --------------------------------------------------------------------
(require 'my-org-fixup)
(require 'my-utils)

;; --------------------------------------------------------------------
;; Key Mapping Section
;; --------------------------------------------------------------------

;; Swap Command key and Option key in GUI mode.
;; It seems that only M-TAB doesn't works.
;; https://apple.stackexchange.com/questions/291194/how-do-i-make-cmd-as-meta-instead-of-option-alt-key-on-emacs-runs-on-ter
(if (display-graphic-p)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super))

;; Scroll 1 line up/down
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))

;; Remap help-map with M-h instead of C-h, make C-h to be translated to DEL.
;; Note that C-M-h will be translated to M-DEL correctly under terminals, but
;; incorrectly under GUI version emacs. So bind it explicitly.
;; See: <https://www.emacswiki.org/emacs/BackspaceKey>
(global-set-key (kbd "M-h") help-map)
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-M-h") 'backward-kill-word)  ;; for compatibility of GUI version

;; 
(global-set-key (kbd "C-x C-h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "M-s O") 'occur)
(global-set-key (kbd "M-s s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-s M-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-s h h") 'highlight-symbol-at-point)

(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "C-]") 'helm-gtags-find-tag-from-here)
     (define-key helm-gtags-mode-map (kbd "C-c s t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "C-c s r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "C-c s s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "C-c s g") 'helm-gtags-find-pattern)
     (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)))

;; yasnippet
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;;(define-key yas-minor-mode-map (kbd "M-?") 'yas-expand)

(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
              (global-set-key [double-wheel-right] (lambda () (interactive) (scroll-left 2)))
              (global-set-key [trible-wheel-right] (lambda () (interactive) (scroll-left 4)))

              (global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))
              (global-set-key [double-wheel-left] (lambda () (interactive) (scroll-right 2)))
              (global-set-key [trible-wheel-left] (lambda () (interactive) (scroll-right 4))))))

(progn
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up 2)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up 4)))
  (global-set-key [trible-wheel-down] (lambda () (interactive) (scroll-up 32)))

  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down 4)))
  (global-set-key [trible-wheel-up] (lambda () (interactive) (scroll-down 32))))


;; --------------------------------------------------------------------
;; Other Configurations
;; --------------------------------------------------------------------

;; Don't show welcome buffer
(setq inhibit-startup-message t)

;; Update gtags database when source code changes
(setq helm-gtags-auto-update t)

;; Line number format
(setq linum-format "%4d  ")

;; Show matching parenthesis
(show-paren-mode 1)

;; Case sensitive completion
(setq completion-ignore-case nil)

;; Show column number in status bar
(setq column-number-mode t)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; Coding Style
(setq c-default-style "linux" c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Set up babel support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

;; Need gxref package

(require 'gxref)
(defun gxref-xref-backend-fallthrough ()
  "Gxref backend for Xref. If no Gtags database found, fall
  through and use next backend"
  (if (gxref--find-project-root) 'gxref nil))

(add-to-list 'xref-backend-functions 'gxref-xref-backend-fallthrough)

;; Set Org mode executable languages
;; see http://orgmode.org/org.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (gnuplot . t)
   (ditaa . t)
   (plantuml . t)
   (sh . t)
   (dot . t)
   (R . t)))

;; Set ditaa.jar location
(setq org-ditaa-jar-path "~/.emacs.d/java/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/java/plantuml.jar")

;; --------------------------------------------------------------------
;; Modes Activations
;; --------------------------------------------------------------------

(ido-mode)
(helm-mode)
(global-linum-mode)
(yas-global-mode 1)



;; --------------------------------------------------------------------
;; Custom Variables
;; --------------------------------------------------------------------
(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 3) ((control)))))

 ;; Speedbar don't shown unknown format files, such as *.proto, you may
 ;; not find them in speedbar without this variable turn on, which may
 ;; make you consfused. To temporarily turn off this flag, you can uncheck
 ;; the menu item in 'Speedbar > Show All Files', or 'M-x customize' and
 ;; search "speedbar show unknow".
 '(speedbar-show-unknown-files t)

 ;; You can use this to (re)install packages on other machines by
 ;; running ‘package-install-selected-packages’
 '(package-selected-packages
   (quote
    (async markdown-mode yasnippet linum-off helm-gtags gxref gnuplot gnuplot-mode))))


;; --------------------------------------------------------------------
;; Custom Faces
;; --------------------------------------------------------------------
(custom-set-faces
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-refine-added ((t (:inherit diff-refine-change))))
 '(diff-refine-change ((t (:weight bold))))
 '(diff-refine-changed ((t (:inverse-video t))))
 '(diff-refine-removed ((t (:inherit diff-refine-change))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(font-lock-builtin-face ((t (:foreground "white"))))
 '(helm-buffer-directory ((t (:background "black" :foreground "#5c5cff"))))
 '(helm-ff-directory ((t (:background "black" :foreground "#5c5cff"))))
 '(helm-selection ((t (:background "#3a3a3a" :underline t))))
 '(org-table ((t (:foreground "green"))))
 '(region ((t (:background "#00008e")))))

(if (display-graphic-p)
    ;; for GUI
    (custom-set-faces
     ;; SimHei (仿黑) 是Mac下面的一种中文正好是英文两倍宽的字体.
     '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "SimHei")))))

  ;; for non-GUI
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "default" :family "Monaco"))))))

(provide 'super-init)
