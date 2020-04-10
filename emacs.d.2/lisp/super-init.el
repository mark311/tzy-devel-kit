(require 'package)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; --------------------------------------------------------------------
;; Require & Load Libraries
;; --------------------------------------------------------------------
(require 'helm-config)  ;; package helm
(require 'linum-off)    ;; package linum-off
(require 'yasnippet)    ;; package yasnippet

;; --------------------------------------------------------------------
;; Personal Scripts
;; --------------------------------------------------------------------
(require 'my-org-fixup)
(require 'my-utils)

;; --------------------------------------------------------------------
;; 操作型的全局键位绑定
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
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "M-s O") 'occur)
(global-set-key (kbd "M-s s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-s M-s") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-s h h") 'highlight-symbol-at-point)
(global-set-key (kbd "M-(") 'my/insert-parentheses)

;; expand
(global-set-key (kbd "M-s-/") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'company-complete)

;; --------------------------------------------------------------------
;; 功能型的全局键位绑定      C-c <letter>
;;
;; - 窗口相关功能           C-c w
;; - 文件相关功能           C-c f
;; - 项目相关功能           C-c p
;; - Git相关功能            C-c g
;; --------------------------------------------------------------------

;; speedbar激活的时候，右手一般预先已经挪到了鼠标或触摸板上，激活的操
;; 作需要左手单手快速完成
(global-set-key (kbd "C-c w w") 'speedbar-get-focus)

;; 激活magit的主界面
(global-set-key (kbd "C-c w g") 'magit-status) ; deprecated

;; revert-buffer
(global-set-key (kbd "C-c f r") 'revert-buffer)

;; delete white spaces
(global-set-key (kbd "C-c f w") 'delete-trailing-whitespace)

;; 调用recompile进行项目编译构建
(global-set-key (kbd "C-c p c") 'my/recompile)  ; deprecated
(global-set-key (kbd "C-c p C") 'rsync-compile) ; deprecated
(global-set-key (kbd "C-c c c") 'my/recompile)
(global-set-key (kbd "C-c c C") 'rsync-compile)
(global-set-key (kbd "C-c c e") 'my/goto-first-compile-error)
(global-set-key (kbd "C-c c k") 'my/kill-compilation)

;; magit相关
(global-set-key (kbd "C-c g g") 'magit-status)
(global-set-key (kbd "C-c g u") 'magit-diff-unstaged)
(global-set-key (kbd "C-c g s") 'magit-diff-staged)
(global-set-key (kbd "C-c g b") 'magit-branch-popup)
(global-set-key (kbd "C-c g B") 'magit-blame)
(global-set-key (kbd "C-c g l") 'magit-log-popup)
(global-set-key (kbd "C-c g L") 'magit-log-buffer-file)

;; yas-snippets相关
(global-set-key (kbd "C-M-/") 'yas-expand)
(global-set-key (kbd "C-c y s") 'yas-insert-snippet)
(global-set-key (kbd "C-c y n") 'yas-new-snippet)

;; --------------------------------------------------------------------
;; Mode局部键位绑定
;; --------------------------------------------------------------------

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

;; Since emacs 26.1, C-M-h is bind to another c/c++-mode function. It
;; overwrites my pre-defined action, which is 'backward-kill-word.
(add-hook 'c-mode-common-hook
          (lambda ()
            (progn
               (define-key c++-mode-map (kbd "C-M-h") nil)
               (define-key c-mode-map (kbd "C-M-h") nil)
               (define-key java-mode-map (kbd "C-M-h") nil)
               (define-key awk-mode-map (kbd "C-M-h") nil))))

;; org mode
(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
              (global-set-key [double-wheel-right] (lambda () (interactive) (scroll-left 2)))
              (global-set-key [trible-wheel-right] (lambda () (interactive) (scroll-left 4)))

              (global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))
              (global-set-key [double-wheel-left] (lambda () (interactive) (scroll-right 2)))
              (global-set-key [trible-wheel-left] (lambda () (interactive) (scroll-right 4))))))

;; mouse
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

;; Set Org mode executable languages
;; see http://orgmode.org/org.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (gnuplot . t)
   (ditaa . t)
   (plantuml . t)
   ;; Commented for Emacs26.1 doesn't has 'sh' language, maybe other
   ;; name instead of 'sh'.
   ;(sh . t)
   (dot . t)
   (R . t)))

;; Set ditaa.jar location
(setq org-ditaa-jar-path "~/.emacs.d/java/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/java/plantuml.jar")

;; markdown-preview css
(setq markdown-preview-stylesheets
      ;; edit markdown-preview.css at
      ;; "emacs.d.2/misc/markdown-preview.css" under this git repo
      (list "http://markta31.oss-cn-hangzhou.aliyuncs.com/share/markdown-preview.css"))

;; --------------------------------------------------------------------
;; Modes Activations
;; --------------------------------------------------------------------

(ido-mode)
(helm-mode)
(global-linum-mode)
(yas-global-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

;; --------------------------------------------------------------------
;; Custom Variables
;; --------------------------------------------------------------------
(custom-set-variables
 ;; Default themes
 '(custom-enabled-themes (quote (zhiyang-v1)))

 ;; To make dabbrev-expand case sensitive. For details, see *info*
 ;; page of emacs: (emacs)Top > Abbrevs > Dabbrev Customization
 '(dabbrev-case-replace nil)
 '(dabbrev-case-fold-search nil)

 ;; company-mode configuration
 '(company-auto-complete t)
 '(company-backends (quote (company-dabbrev company-etags)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-show-numbers (quote (quote t)))

 ;; In GUI mode, don't show scroll-bar and tool-bar, make GUI cleaner.
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)

 '(ediff-split-window-function (quote split-window-horizontally))

 ;; Make Trackpad scrolling slower
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 3) ((control)))))

 ;; Speedbar don't shown unknown format files, such as *.proto, you may
 ;; not find them in speedbar without this variable turn on, which may
 ;; make you consfused. To temporarily turn off this flag, you can uncheck
 ;; the menu item in 'Speedbar > Show All Files', or 'M-x customize' and
 ;; search "speedbar show unknow".
 '(speedbar-show-unknown-files t)

 ;; Speedbar: don't show an icon in the left of each line, use acsii characters
 ;; instead.
 '(speedbar-use-images nil)

 ;; Speedbar: set bigger initial width
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 35)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))

 ;; Speedbar: 设置能进一步分析文件内部结构的文件名后缀。（默认值缺少对
 ;; go的支持，因此修改这个值）
 '(speedbar-supported-extension-expressions
   (quote
    (".go" ".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?")))

 ;; 删掉了与c/c++相关的默认的配置，仅仅保留cchh一项。目的是为了让cchh
 ;; 被优先匹配到，而不需要手动输入。
 ;;
 ;;     ("ch" .    "*.[ch]")
 ;;     ("c" .     "*.c")
 ;;     ("cc" .    "*.cc *.cxx *.cpp *.C *.CC *.c++")
 ;;   * ("cchh" .  "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
 ;;     ("hh" .    "*.hxx *.hpp *.[Hh] *.HH *.h++")
 ;;     ("h" .     "*.h")
 ;;
 '(grep-files-aliases
   (quote
    (("all" . "* .[!.]* ..?*")
     ("el" . "*.el")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]"))))

 ;; markdown processor
 ;; downloaded at https://daringfireball.net/projects/markdown/
 '(markdown-command "~/.emacs.d/misc/Markdown_1.0.1/Markdown.pl")
 )

;; --------------------------------------------------------------------
;; Custom Faces
;; --------------------------------------------------------------------
(custom-set-faces
 ;; Monaco 14px 是一种相对圆润的字体，与我配置的Iterm2中的字体保持一致
 '(default ((t (:height 140 :width normal))))
 )

(provide 'super-init)
