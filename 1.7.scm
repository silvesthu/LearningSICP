#lang scheme

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess last-guess)
  (< (abs (- guess last-guess)) 0.0001))

(define (square x) (* x x))

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess)
          guess
          (sqrt-iter (improve guess x) guess x)))

(define (sqrt x) (sqrt-iter 1.0 0 x))

(newline)
; (sqrt-iter 0.05 0.0001) ; bad epsilon
; (square 0.01)

(sqrt 2)
(sqrt 0.0001)

