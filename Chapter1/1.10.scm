#lang scheme

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
        	     (A x (- y 1))))))

(A 0 1) ; 2
(A 0 10) ; 20
(A 0 20) ; 40

; Answer : 2 * n

; (A 0 n) -> 2 * n

(A 1 10) ; 1024
(A 1 9)

; Answer : 2^n

; (A 1 n) -> (A 0 (A 1 (- n 1)) -> ... -> (A 0 (A 0 (A 0 (... (A 1 1)))))
; and (A 1 1) -> 2
; => (A 1 n) -> 2 * 2 * 2 * ... * 2 -> 2 ^ n

(newline)

(A 2 0) ; 0 
(A 2 1) ; 2
(A 2 2) ; 4
(A 2 3) ; 16
(A 2 4) ; 65536
(A 2 5) ; ~

(newline)

; Answer : 2 ↑↑ n
; which is 2 ^ (2 ^ (2 ^ (2 ^ ....))) with count of '2' = n

; !!!! be cautious about exponetional arithmetics !!!!
; 2 ↑↑ n can not be represented by other simpler form
; that's why special symbols as ↑↑ is needed
; REF : Knuth's up-arrow notation (http://en.wikipedia.org/wiki/Knuth%27s_up-arrow_notation)

; >> [TODO] think about..

(A 3 1) ; 2
(A 3 2) ; 4
(A 3 3) ; 65536

