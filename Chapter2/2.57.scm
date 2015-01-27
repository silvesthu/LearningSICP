#lang scheme

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentitation x y) (list '** x y))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (cond 
         	((eq? (exponent exp) 0) 1)
         	((eq? (exponent exp) 1) (base exp))
         	(else (make-product (exponent exp) (make-exponentitation (base exp) (list '- (exponent exp) 1))))
         )
        )
        (else
         (error "unknown expression type - DERIV" exp))))

; ---------- code above copy from https://mitpress.mit.edu/sicp/full-text/sicp/book/node39.html ------------

(define (make-sum a1 a2) (list '+ a1 a2))
(define (addend s) (cadr s))
(define (augend s) 
	(cond 
		((eq? (cdddr s) '()) (caddr s))
		(else (make-sum (caddr s) (cadddr s)))
	))

(define (make-product m1 m2) (list '* m1 m2))
(define (multiplier p) (cadr p))
(define (multiplicand p) (cond 
		((eq? (cdddr p) '()) (caddr p))
		(else (make-product (caddr p) (cadddr p)))
	))

(deriv '(+ x (* x 5) (* x x)) 'x)
(deriv '(* x y (+ x 3)) 'x) ; seems correct.. but quite noisy...
