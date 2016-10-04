#lang racket
(require racket/stream)

(include "lazy.scm")

(stream-cons 1 2)
; (define (f) (stream-cons x y)) ; gives error

(newline)

; with modification of lazy.scm
; only force when variables are displayed

(eval@ '(begin 
  (define (cons x y)
    (lambda (m) (m x y)))

  (define (car z)
    (z (lambda (p q) p)))

  (define (cdr z)
    (z (lambda (p q) q)))


  (define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

  (define (map proc items)
    (if (null? items)
        '()
        (cons (proc (car items))
              (map proc (cdr items)))))

  (define (scale-list items factor)
    (map (lambda (x) (* x factor))
         items))

  (define (add-lists list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (cons (+ (car list1) (car list2))
                      (add-lists (cdr list1) (cdr list2))))))
) the-global-environment)

(eval@ '(begin 

(define xy (cons x y))

(newline)

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(display (list-ref integers 17))

) the-global-environment)
