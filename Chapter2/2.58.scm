#lang scheme

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentitation x y) (list '** x y))

(define (deriv exp var)
  (display "deriving ") (display exp) (newline)
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

(define (unwrap l)
	(cond 
		((and (pair? l) (eq? (cdr l) '())) (unwrap (car l)))
		(else l)
	)
)

(define (make-sum a1 a2) (list a1 '+ a2))
(define (make-product m1 m2) (list m1 '* m2))

(define (plus? x) (eq? x '+))
(define (plus?-not x) (not (plus? x)))
(define (sum? x)
  	(define ux (unwrap x))
	(and 
		(pair? ux)
		(plus?-not (car ux))
		(not (null? (filter plus? ux)))
		(plus?-not (last ux))
	)
)
(define (addend s)
	(unwrap (cdr (dropf (unwrap s) plus?-not)))
)
(define (augend s)
	(unwrap (takef (unwrap s) plus?-not))
)

(define (product? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define (multiplier p) (unwrap (car p)))
(define (multiplicand p) (unwrap (cddr p)))

(sum? (make-sum 1 2))
(addend (make-sum 1 2))

; a.
;(deriv '(x + (3 * (x + (y + 2)))) 'x)

; b.

(define t '(x + 3 * (x * y + 2 * x)))
;(deriv '(x + 3 * (x + y + 2)) 'x)
;(deriv '(x + x * 5) 'x) 

(define target '(x * 5 + 5))
;(deriv target 'x) ; not good

; in deriv, sum is already above product
; so find fisrt "+" in top layer from  pattern like ((1 * 2) + 1 * 2) should be enough

(deriv target 'x) ; good now
(deriv t 'x)
; -> (((3 * (((2 * 1) + (0 * x)) + ((x * 0) + (1 * y)))) + (0 * (x * y + 2 * x))) + 1)
; -> 3(y+2)+1 by wolframalpha



