;; See section "中文中，转成HTML时，换行符成空格符号" in
;; http://wiki.dreamrunner.org/public_html/Emacs/org-mode.html
;;
(defadvice org-html-paragraph (before org-html-paragraph-advice
                                      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let* ((origin-contents (ad-get-arg 1))
         (fix-regexp "[[:multibyte:]]")
         (fixed-contents
          (replace-regexp-in-string
           (concat
            "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))

    (ad-set-arg 1 fixed-contents)))


;; 这个函数的原始实现会在(apply 'max color-values)的地方报错，原因是
;; color-values变量是nil，nil的原因的是参数list（defined-colors函数返
;; 回的color names）中包含了某些color name，它们是没有color values的，
;; 也就是说对这些color name调用(color-values)函数会返回nil，所以会导致
;; 执行失败，最终显示wrong-number-of-arguments的错误。
;;
;; 修复方法：在该函数中添加了一行，把list中(color-values (car color))
;; 返回nil的项给剔除掉。
(defun list-colors-print (list &optional callback)
  (let ((callback-fn
	 (if callback
	     `(lambda (button)
		(funcall ,callback (button-get button 'color-name))))))

    ;; 仅仅添加了下面一行
    (setq list (seq-filter (lambda (color) (color-values (car color))) list))

    (dolist (color list)
      (if (consp color)
	  (if (cdr color)
	      (setq color (sort color (lambda (a b)
					(string< (downcase a)
						 (downcase b))))))
	(setq color (list color)))
      (let* ((opoint (point))
	     (color-values (color-values (car color)))
	     (light-p (>= (apply 'max color-values)
			  (* (car (color-values "white")) .5))))
	(insert (car color))
	(indent-to 22)
	(put-text-property opoint (point) 'face `(:background ,(car color)))
	(put-text-property
	 (prog1 (point)
	   (insert " ")
	   ;; Insert all color names.
	   (insert (mapconcat 'identity color ",")))
	 (point)
	 'face (list :foreground (car color)))
	(insert (propertize " " 'display '(space :align-to (- right 9))))
	(insert " ")
	(insert (propertize
		 (apply 'format "#%02x%02x%02x"
			(mapcar (lambda (c) (lsh c -8))
				color-values))
		 'mouse-face 'highlight
		 'help-echo
		 (let ((hsv (apply 'color-rgb-to-hsv
				   (color-name-to-rgb (car color)))))
		   (format "H:%.2f S:%.2f V:%.2f"
			   (nth 0 hsv) (nth 1 hsv) (nth 2 hsv)))))
	(when callback
	  (make-text-button
	   opoint (point)
	   'follow-link t
	   'mouse-face (list :background (car color)
			     :foreground (if light-p "black" "white"))
	   'color-name (car color)
	   'action callback-fn)))
      (insert "\n"))
    (goto-char (point-min))))


(provide 'my-org-fixup)
