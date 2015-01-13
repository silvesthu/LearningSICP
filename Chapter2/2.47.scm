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

(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2)
)

(define (make-frame-c origin edge1 edge2)
	(cons origin (cons edge1 edge2))
)

(define (origin-frame frame) (car frame))
(define (origin-frame-c frame) (car frame))

(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))
(define (edge1-frame-c frame) (cadr frame))
(define (edge2-frame-c frame) (cddr frame))

(newline)

(origin-frame (make-frame (make-vect 1 2) (make-vect 1 3) (make-vect 1 4)))
(edge1-frame (make-frame (make-vect 1 2) (make-vect 1 3) (make-vect 1 4)))
(edge2-frame (make-frame (make-vect 1 2) (make-vect 1 3) (make-vect 1 4)))

(newline)

(origin-frame-c (make-frame-c (make-vect 1 2) (make-vect 1 3) (make-vect 1 4)))
(edge1-frame-c (make-frame-c (make-vect 1 2) (make-vect 1 3) (make-vect 1 4)))
(edge2-frame-c (make-frame-c (make-vect 1 2) (make-vect 1 3) (make-vect 1 4)))