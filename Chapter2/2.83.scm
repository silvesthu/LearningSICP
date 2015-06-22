#lang scheme

; raise in type tower

; integer(number), rational, real, complex

(include "./algebra.scm")

(define (install-real-package) ; e.g. use inexact to represent real
  (define (tag x) (attach-tag-force 'real x)) ; conflict with number.... use an alternative attach-tag...
  (put 'make 'real
       (lambda (n) (tag n)))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-raise-package)
	(put 'raise 'number
       (lambda (i) (make-rational i 1)))
	(put 'raise 'rational
       (lambda (i) (make-real (/ (car i) (cdr i)))))
	(put 'raise 'real
       (lambda (i) (make-complex-from-real-imag i 0)))
)

(install-raise-package)

(apply-generic 'raise 7)
(apply-generic 'raise (apply-generic 'raise 7))
(apply-generic 'raise (apply-generic 'raise (apply-generic 'raise 7)))

(newline)