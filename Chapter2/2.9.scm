#lang scheme

(include "interval.scm")

(define (width n)
	(/ (- (upper-bound n) (lower-bound n)) 2))

; (width (add-interval x y))

;(width ((make-interval 	
;			(+ (lower-bound x) (lower-bound y))
;			(+ (upper-bound x) (upper-bound y))
;	) x y))

; (/ (- (+ (upper-bound x) (upper-bound y)) (+ (lower-bound x) (lower-bound y))) 2)

; (/ (+ (- (upper-bound x) (lower-bound x)) (- (upper-bound y) (lower-bound y)))) 2)

; (+ (width x) (width y))

; similiar on substract

; But how to prove some function is not exist ...

; mul

(width (make-interval 2 2))
(width (make-interval 0 2))
(width (mul-interval (make-interval 2 2) (make-interval 0 2)))

; div -> there is div-zero error