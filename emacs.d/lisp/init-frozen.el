(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.emacs-china.org/gnu/")
	("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

(setq use-package-always-ensure t)	; install package if not exists
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package projectile)
(use-package flycheck)

;; usage: 1. use TAB to expand abbravs
;;        2. use C-c & ... to create / insert / visit snippets
(use-package yasnippet
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/yasnippets" "~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :init
  (custom-set-variables
   '(lsp-keymap-prefix "C-c l"))
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  (global-set-key (kbd "M-RET") 'lsp-execute-code-action))

(use-package hydra)

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  :custom
  (dap-java-test-runner
   "~/.emacs.d/.cache/lsp/eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar")
  :bind
  (("C-c r m" . dap-java-run-test-method)
   ("C-c r c" . dap-java-run-test-class)))

(use-package dap-java :ensure nil)
(use-package helm-lsp
  :bind
  (("C-x C-d" . helm-lsp-workspace-symbol)))

(use-package helm
  :config
  (helm-mode)
  ;;
  ;; Use helm-mode and ido-mode
  ;;
  ;; To use Ido for some commands and Helm for others, do not enable
  ;; ido-mode. Instead, customize helm-completing-read-handlers-alist
  ;; to specify which command uses Ido.
  ;;
  ;; see [https://github.com/emacs-helm/helm/wiki#use-helm-mode-and-ido-mode]
  ;;
  (setf (alist-get 'find-file helm-completing-read-handlers-alist) 'ido)
  (setf (alist-get 'switch-to-buffer helm-completing-read-handlers-alist) 'ido)
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x f" . find-file)		; will use ido-find-file
   ("C-x b" . switch-to-buffer)		; will use ido-switch-buffer
   ("M-s o" . helm-occur)
   ("M-s O" . occur)))

(use-package lsp-treemacs)

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-dispatch-popup)
  (global-set-key (kbd "C-x g") 'magit-status))

;; usage:
;;   M-x markdown-preview-mode
;;
(use-package markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets
	;; edit markdown-preview.css at
	;; "emacs.d.2/misc/markdown-preview.css" under this git repo
	(list "http://markta31.oss-cn-hangzhou.aliyuncs.com/share/markdown-preview.css"))
  )

(use-package markdown-mode
  :config
  (custom-set-variables
   ;; markdown processor
   ;; downloaded at https://daringfireball.net/projects/markdown/
   '(markdown-command "~/.emacs.d/misc/Markdown_1.0.1/Markdown.pl")
   ))

(require 'my-utils)
(require 'rsync-compile)
(require 'ansi-color)

;; ------------------------------------------------------------
;; Settings
;; ------------------------------------------------------------
(progn

  ;; ------------------------------
  ;; Basic Key Bindings
  ;; ------------------------------
  (progn
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

    ;; Since emacs 26.1, C-M-h is bind to another c/c++-mode function. It
    ;; overwrites my pre-defined action, which is 'backward-kill-word.
    (add-hook 'c-mode-common-hook
              (lambda ()
		(progn
		  (define-key c++-mode-map (kbd "C-M-h") nil)
		  (define-key c-mode-map (kbd "C-M-h") nil)
		  (define-key java-mode-map (kbd "C-M-h") nil)
		  (define-key awk-mode-map (kbd "C-M-h") nil))))

    ;; avoid closing Emacs unintentionaly
    (global-unset-key (kbd "C-x C-c"))

    (global-set-key (kbd "C-x C-o") 'other-window)

    (global-set-key (kbd "M-s s") 'isearch-forward-symbol-at-point)
    (global-set-key (kbd "M-s M-s") 'isearch-forward-symbol-at-point)

    (global-set-key (kbd "M-s h h") 'highlight-symbol-at-point)

    ;; execute bash command in *.sh or *.txt
    (add-hook 'sh-mode-hook
	      (lambda () (local-set-key (kbd "C-x C-e") 'my/execute-shell-command)))
    (add-hook 'text-mode-hook
	      (lambda () (local-set-key (kbd "C-x C-e") 'my/execute-shell-command)))

    nil)

  ;; ------------------------------
  ;; Common UI
  ;; ------------------------------
  (progn
    ;; Show matching parenthesis
    (show-paren-mode 1)

    ;; Show column number in status bar
    (setq column-number-mode t)

    ;; Show line number for all programatic mode
    (add-hook 'prog-mode-hook (lambda () (linum-mode)))

    (custom-set-variables

     ;; In GUI mode, don't show scroll-bar and tool-bar, make it cleaner.
     '(scroll-bar-mode nil)
     '(tool-bar-mode nil)

     ;; 高亮显示行尾空格字符
     '(show-trailing-whitespace t))

    )

  ;; ------------------------------
  ;; iSearch & Replace & Dabbrev
  ;; ------------------------------
  (progn

    (custom-set-variables
     ;; To make dabbrev-expand case sensitive. For details, see *info*
     ;; page of emacs: (emacs)Top > Abbrevs > Dabbrev Customization
     '(dabbrev-case-replace nil)
     '(dabbrev-case-fold-search nil)

     '(company-dabbrev-downcase nil)
     '(company-dabbrev-ignore-case t)
     '(company-dabbrev-char-regexp "\\sw\\|\\s_") ; allow search word characters and underscope (_)

     )
    )

  ;; ------------------------------
  ;; Coding Style
  ;; ------------------------------
  (add-hook 'c++-mode-hook (lambda ()
                             (setq c-default-style "linux"
                                   c-basic-offset 4
                                   indent-tabs-mode nil)))

  (add-hook 'c-mode-hook (lambda ()
                           (setq c-default-style "linux"
                                 c-basic-offset 4
                                 indent-tabs-mode nil)))

  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 4
                                    indent-tabs-mode nil)))

  ;; ------------------------------
  ;; Compilation
  ;; ------------------------------
  (progn
    (custom-set-variables
     ;; `compile' / `recompile' function calls `display-buffer' to show
     ;; *compilation* buffer, the following `display-buffer-alist' will
     ;; affect the behavior of choosing frames to display.
     ;;
     ;; In details, for buffer named *compilation*, search all frames to
     ;; find one which contains the *compilation* buffer already,
     ;; otherwise starts a window in current selected frame.
     '(display-buffer-alist
       (quote
	(("\\*compilation\\*" display-buffer-reuse-window
	  (reusable-frames . t)))))

     ;; To make the *compilation* buffer scrolling to as output appears
     ;; and stop scrolling at first error occur.
     '(compilation-scroll-output (quote first-error)))

    ;; 让有颜色的terminal输出能以彩色模式显示
    (add-hook 'compilation-filter-hook
	      (lambda () (ansi-color-apply-on-region compilation-filter-start (point))))
    )

  ;; ------------------------------
  ;; RsyncCompile
  ;; ------------------------------
  (progn
    ;; 调用rsyncompile进行项目编译构建
    (global-set-key (kbd "C-c c c") 'rsync-compile-last)
    (global-set-key (kbd "C-c c C") 'rsync-compile-prompt)
    )

  nil)


(provide 'init-frozen)
