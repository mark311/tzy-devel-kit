
;; My customized Java style
;;
;; Based on built-in "java" style, some fields are modifed to make it
;; more like the style of IDE (Eclipse / Intelli-IDEA).
;;
;; `arglist-cont' and `arglist-cont-nonempty' is changed to `++',
;; which means 2 times of c-basic-offset (8 chars typically)
;;
;; Other fields of `c-offsets-alist' keeps the same with that of the
;; built-in "java" style.
;;
;; For more information about configuration of `c-offsets-alist', see
;; Info pages:
;;
;;   - (ccmode)Top > Customizing Indentation > c-offsets-alist"
;;   - (ccmode)Top > Indentation Engine Basics > Syntactic Symbols"
;;
(defconst my/java-style-description
  '((c-basic-offset . 4)
    (c-offsets-alist . ((inline-open . 0)
			(topmost-intro-cont . +)
			(statement-block-intro . +)
			(knr-argdecl-intro . 5)
			(substatement-open . +)
			(substatement-label . +)
			(label . +)
			(statement-case-open . +)
			(statement-cont . ++)

			(arglist-intro . ++)
			(arglist-cont . ++)
			(arglist-cont-nonempty . ++)
			(arglist-close . 0)

			(access-label . 0)
			(inher-cont . c-lineup-java-inher)
			(func-decl-cont . c-lineup-java-throws)))))

(provide 'my-styles)
