#lang scheme

; recursive process (directly from definition)

(define (f n)
  (if (< n 3) 
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 10)

; version 1

(define (fi n)
  (if (< n 3)
      n
      (g 2 1 0 (- n 3))))

(define (g n2 n1 n0 counter)
  (if (= counter 0)
      (+ n2 (* n1 2) (* n0 3))
      (g (+ n2 (* n1 2) (* n0 3)) n2 n1 (- counter 1))))
(f 10) ; test compare
(fi 10) ; test

; version 2 (eliminate duplication)

(define (fi2 n)
  (if (< n 3)
      n
      (g2 2 1 0 (- n 2)))) ; v2.1: minus 1 here

(define (g2 n2 n1 n0 counter)
  (if (= counter 0)
      n2 ; v2.2: an extra loop
      (g2 (+ n2 (* n1 2) (* n0 3)) n2 n1 (- counter 1))))

(f 20) ; test compare
(fi 20) ; test compare
(fi2 20) ; test