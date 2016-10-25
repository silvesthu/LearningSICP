#lang racket

(include "lazy.scm")

(eval@ '
  (begin 
    (define (cons x y)
      (lambda (m) (m x y)))

    (define (car z)
      (z (lambda (p q) p)))

    (define (cdr z)
      (z (lambda (p q) q)))

    (define (cadr z)
      (car (cdr z)))

    (define (caadr z)
      (car (car (cdr z))))

    (define (caddr z)
      (car (cdr (cdr z))))

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

    ;(display 'a)
    ;(display '(a b c))
    (display (car '(a b c)))
    (newline)
    (display (car (cdr '(a b c))))
    (newline)
    (display (cadr '(a b c)))
    (newline)
    (display '(a b c)) ; for 4.34 convert lambda-cons back to native list
    ;(display (+ 1 2))
    ;(display (cons a (cons b (cons c '()))))
    ;(display (cons c '()))
    ;(display c)
  )
the-global-environment)

;(display (car '(a b c)))

;(display '(a b c))
;(display (car (quote (a b c))))

;(display (cons a (cons b (cons c '()))))
;(display (cons c '()))
;(display (cons 1 '(c '(d e))))
;(display c)
