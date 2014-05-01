#lang scheme

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (a) (print "a"))
(define (b) (print "b"))

(if (< 1 2) (a) (b)) ; evalute single side

(newline)

(new-if (< 1 2) (a) (b)) ; evalute both side

; -------------------------------------

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x) x)))

(newline)
(sqrt-iter 1 2)
(sqrt-iter 1.0 2)

; (sqrt-iter-new-if 1.0 2) ; infinite loop