#lang scheme

(include "header.scm")

; 1. check '*unassigned*

;(eval@ 
;	'(let (
;			(u '*unassigned*)
;			(v '*unassigned*)
;		)
;		(display u) 
;		(display v)) the-global-environment)

; 2. scan-out-defines

; Recheck procedure with lambda or define are same
;(eval@ 
;	'(let (
;			(u '*unassigned*)
;			(v '*unassigned*)
;		)
;		(define (p1 a) (display a))
;		(display p1) 
;		(newline)
;		(define p2 (lambda (a) (display a)))
;		(display p2)
;		(newline)) the-global-environment)

;(define (filter pred x)
;     	(cond 
;     		((null? x) x)
;     		((pred (car x)) (cons (car x) (filter pred (cdr x))))
;     		(else (filter pred (cdr x)))
;    	)
;    )

;(define (scan-out-defines proc-exp)
;	(define keyword (car proc-exp)) ; 'procedure
;	(define parameters (cadr proc-exp))
;	(define expressions (cddr proc-exp))
;	(define defines (filter (lambda (e) (and (pair? e) (eq? (car e) 'define))) (cddr proc-exp))) 
;	(list keyword parameters (list 
;		'let 
;		(map (lambda (d) (list (cadr d) '*unassigned*)) defines)
;		(append (map (lambda (d) (list 'set! (cadr d) (caddr d))) defines) expressions)))
;)

; test case
; (scan-out-defines '(procedure (x y) (define z (+ x y)) z))

; 3. target case

(eval@ 
	'((lambda (x)
		(define (even? n)
			(if (= n 0)
				true
				(odd? (- n 1))))
		(define (odd? n)
			(if (= n 0)
				false
				(even? (- n 1))))
		(even? x)) 5)
	the-global-environment)

; well... the implementation in header.scm do not extend environment when eval definition...
; but it should be no harm to install it

; and it'll be better to install in make-procedure, cuz procedure body also used in apply
; which will be called every time the procedure is applied