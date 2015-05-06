#lang scheme

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

; a. built-in types, no tag attached.

; b. 

(define table 1)
(set! table '())

; with a simple list of list (not a table actually...)

(define (put op type item) (set! table (append table (list (list op type item)))))
(define (get op type) 
	(define (get-inner op type t)
		;(display t) (newline)
		(cond 
			((null? t) null)
			((and (equal? op (caar t)) (equal? type (cadar t))) (caddar t))
			(else (get-inner op type (cdr t)))
		)
	)
	(get-inner op type table)
)

(define (install-deriv-sum-product)
	(define (make-sum a1 a2) (list '+ a1 a2))
	(define (make-product m1 m2) (list '* m1 m2))
	(define (sum? x)
	  (and (pair? x) (eq? (car x) '+)))

	; slightly different from 2.56.scm
	; retrive oprands in deriv, internal method process oprands only

	(define (addend s) (car s))
	(define (augend s) (cadr s))
	(define (product? x)
	  (and (pair? x) (eq? (car x) '*)))
	(define (multiplier p) (car p))
	(define (multiplicand p) (cadr p))
	(define (deriv-sum opr var)
		(make-sum (deriv (addend opr) var)
            (deriv (augend opr) var))
	)
	(define (deriv-product opr var)
		(make-sum
           (make-product (multiplier opr)
                         (deriv (multiplicand opr) var))
           (make-product (deriv (multiplier opr) var)
                         (multiplicand opr)))
	)

	(put 'deriv '+ deriv-sum)
	(put 'deriv '* deriv-product)
)

(install-deriv-sum-product)

(deriv '(+ x 3) 'x)
(deriv '(+ x (* x x)) 'x)

; c. add rule for exp

(define (install-deriv-exp)
	(define (base e) (car e))
	(define (exponent e) (cadr e))
	(define (make-exponentitation x y) (list '** x y))

	; borrow from product
	(define (make-product m1 m2) (list '* m1 m2))

	(define (deriv-exp opr var)
		(cond 
         	((eq? (exponent opr) 0) 1)
         	((eq? (exponent opr) 1) (base opr))
         	(else (make-product (exponent opr) (make-exponentitation (base opr) (list '- (exponent opr) 1))))
        )
	)

	(put 'deriv '** deriv-exp)
)

; (deriv '(** u (+ n 1)) 'x) ; error here before installation

(install-deriv-exp)

(deriv '(** u (+ n 1)) 'x)

; d.

; ((get (operator exp) 'deriv) (operands exp) var)

; since dispatch table is implemented in list of list...
; only change the order of argument in put/get will be sufficient in this sample