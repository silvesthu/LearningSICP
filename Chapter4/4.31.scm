#lang scheme

(include "lazy.scm")

(eval@ '
	(define (f a (b lazy) c (d lazy-memo))
		(newline) (display "result = ") (display a) (newline)
		a
	)
the-global-environment)

;(eval@ '
;	(display (f 1 (display "b") 3 (display "d")))
;the-global-environment)

; with no change, all arguments are delayed

(define (list-of-delayed?-args vars exps env)
  (if (no-operands? exps)
      '()
      (cons
      	(cond
      		((and (pair? (car vars)) (eq? (cadar vars) 'lazy))(force-it-nonmem (eval-lazy (first-operand exps) env)))
      		((and (pair? (car vars)) (eq? (cadar vars) 'lazy-memo))(force-it-mem (eval-lazy (first-operand exps) env)))
      		(else (delay-it (first-operand exps) env)))
            (list-of-delayed?-args (cdr vars) (rest-operands exps)
                                  env))))

(define (apply-lazy-new procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed?-args (procedure-parameters procedure) arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(set! apply-lazy apply-lazy-new)

(eval@ '
	(display (f (display "--------a---------") (display "--------b---------") (display "--------c---------") (display "--------d---------")))
the-global-environment)
