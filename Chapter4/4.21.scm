#lang scheme

(include "header.scm")

; as the saying goes, "All problems in computer science can be solved by another level of indirection"

; a. some pseudo-code

;def A(n)
;{
;  def B(fact)
;  {
;    fact(fact, n)
;  }
;  def C(ft, k)
;  {
;    if (k == 1)
;      return 1
;    else
;      return k * ft(ft, k - 1)
;  }
;  B(C) => C(C, n) => C(C, n - 1) => ...
;}
;A(10)

; b. 

(define (f x)
  ((lambda (even? odd?)
    (even? even? odd? x))
  (lambda (ev? od? n)
    (if (= n 0) true (od? ev? od? (- n 1))))
  (lambda (ev? od? n)
    (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 11)
(f 100)

; pass down function explicitly to avoid naming problem... 
; it is like take name-table from env then put those names into parameters
