#lang scheme

(include "header.scm")
(include "analyze.scm")

(define (DUMMY)
	(define (analyze-sequence-text exps)
	  (define (sequentially proc1 proc2)
	    (lambda (env) (proc1 env) (proc2 env)))
	  (define (loop first-proc rest-procs)
	    (if (null? rest-procs)
	        first-proc
	        (loop (sequentially first-proc (car rest-procs))
	              (cdr rest-procs))))
	  (let ((procs (map analyze exps)))
	    (if (null? procs)
	        (error "Empty sequence -- ANALYZE"))
	    (loop (car procs) (cdr procs))))

	(define (analyze-sequence-alyssa exps)
		(define (execute-sequence procs env)
			(cond ((null? (cdr procs)) ((car procs) env))
				(else ((car procs) env)
					(execute-sequence (cdr procs) env))))
		(let ((procs (map analyze exps)))
			(if (null? procs)
				(error "Empty sequence -- ANALYZE"))
			(lambda (env) (execute-sequence procs env))))

	(newline)
)

(eval-analyze
	'(
		(let 
			((f (lambda ()(display "1")(display "2")(display "3")(display "4")(display "5"))))
			f
			f
		)
	)
	the-global-environment)

; 1. both give out same result
; 2. as the text suggested, Alyssa's version leave evaluation of sequence to runtime
; 3. for single-expression sequence
;    - Text version give a direct expression call
;    - Alyssa version give a list with one element of expression call
; 4. for 2-expression sequence
;    - Text version give a lambda which calls two expression inside
;    - Alyssa version give a list with two elements waiting to be analyzed