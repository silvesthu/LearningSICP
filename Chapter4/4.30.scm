#lang scheme

(include "lazy.scm")

; a.

; Cy

;(define (eval-sequence exps evn)
;	(cond ((last-exp? exps) (eval (first-exp exps) env))
;		(else (actual-value (first-exp exps) env)
;			(eval-sequence (rest-exps exps) env))))

; Ben

(eval@ '
	(define (for-each proc items)
		(if (null? items)
			'done
			(begin (proc (car items))
				(for-each proc (cdr items)))))
the-global-environment)

(eval@ '
	(for-each (lambda (x) (newline) (display x))
		(list 57 321 88))
the-global-environment)

;; so it's correct...

;; what's the problem ?
;; according to https://wizardbook.wordpress.com/2011/01/06/exercise-4-30/
;; display makes values to be forced

;; b.

;(eval@ '
;(define (p1 x)
;	(set! x (cons x '(2))) x)
;the-global-environment)

;(eval@ '
;(define (p2 x)
;	(define (p e)
;		e
;		x)
;	(p (set! x (cons x '(2)))))
;the-global-environment)

;(eval@ '
;	(p1 1)
;the-global-environment)
;; {1 2} understandable..

;(eval@ '
;	(p2 1)
;the-global-environment)
;; thunk of 1... => e is not forced, set! is not called

;; with Cy ver. both outputs {1 2}

;; c.
;; as in a.

;; d.
;; more like preference ?
;; i go for approach in the text as it is more lazy... as long as one don't mess with mutatable variables





