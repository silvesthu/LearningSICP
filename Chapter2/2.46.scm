#lang scheme
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s) (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))

(make-vect 1 2)
(add-vect (make-vect 1 2) (make-vect 1 2))
(scale-vect (make-vect 1 2) 2)