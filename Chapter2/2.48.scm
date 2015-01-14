#lang scheme
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s) (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))


(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car v1))
(define (end-segment s) (cdr v1))