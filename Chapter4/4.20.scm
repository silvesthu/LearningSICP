#lang scheme

(include "header.scm")

; a. I thought... implementation for 4.18 works ported from 4.6 also works here
;    but that is not letrec, even not a correct let, it give result same as let* in native scheme of racket

;    to match let in racket impl, let->lambda should be modified as name-value-pairs are evaluated with some temporary names
;    after evaluation of that part, those names are then registered into env

(define (letrec->lambda exp)
  (define clauses (cdr exp))
  (if (null? clauses)
      'false
      (let (
          (raw-defines (car clauses))
          (expressions (cdr clauses))
        )
     	(define null-defines (map (lambda (d) (list 'define (car d) ''*unassgined*)) raw-defines))  ; define as unassigned first
        (define sets (map (lambda (d) (cons 'set! d)) raw-defines))									; then set!
        (display (sequence->exp (append null-defines sets expressions)))
        (sequence->exp (append null-defines sets expressions))
      ))
)

(put-keyword 'letrec (lambda (exp env) (eval@ (letrec->lambda exp) env)))

;(eval@ 
;	'(letrec ((fact
;		(lambda (n)
;			(if (= n 1)
;				1
;				(* n (fact (- n 1)))))))
;	(fact 10))
;	the-global-environment)

;(eval@ 
;	'(letrec 
;			((is-even? (lambda (n)
;                       (or (= n 0)
;                           (is-odd? (- n 1)))))
;            (is-odd? (lambda (n)
;                       (if (= n 0)
;                       	#f
;                       	(is-even? (- n 1))))))                           
;    (is-odd? 11))
;	the-global-environment)

((lambda ()
(begin
(define a 1)
(define b 2)
(display (let
	(
		(a b)
		(b a) ; let -> 3
	) (+ a b)))
(display ", ")
(display (let*
	(
		(a b)
		(b a) ; let* -> 4
	) (+ a b)))
;(letrec
;	(
;		(a b)
;		(b a) ; letrec -> error
;	) (+ a b))
)))

;(newline)
;(begin 
;	(define a 1)
;	(define b 2)
;	((lambda ()
;	(define a '*unassgined*)
;	(define b '*unassgined*) 
;	(set! a b) ; give error
;	(set! b a) 
;	(+ a b)))
;)

;(newline)
;(eval@ 
;	'(begin
;		(define a 1)
;		(define b 2)
;		(letrec
;			(
;				(a b) ; give error as expected
;				(b a)
;			)
;		(+ a b))
;	)
;	the-global-environment)

; --------------------------------------------------

; b. error will occur as the line below

; (define even?-in-4.20 (lambda () (odd?-in-4.20))) ; odd?-in-4.20 is unbound identifier when evaluated this