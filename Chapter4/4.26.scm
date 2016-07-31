#lang scheme

(include "header.scm")

(define (unless->if exp)
	(define condition (cadr exp))
	(define usual-value (caddr exp))
	(define exceptional-value (cadddr exp))
	(make-if 
		condition
		exceptional-value
		usual-value)
)

(put-keyword 'unless (lambda (exp env) (eval@ (unless->if exp) env)))

(eval@
	; as a special form
	'(unless (> 2 3) 1 2)
	the-global-environment)

; not sure about the necessarility
; if is a special form it self...

; someone can create a compond condition expression like
; (and (op x x1 x2) (op y y1 y2)) 
; pass unless or if or other "selector" to it
