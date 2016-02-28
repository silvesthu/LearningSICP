#lang racket
(require racket/stream)

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                    (interleave s2 (stream-rest s1)))
  )
)

(define (pairs s t)
  (stream-cons
    (list (stream-first s) (stream-first t))
    (interleave
      (stream-map (lambda (x) (list (stream-first s) x))
        (stream-rest t)
      )
      (pairs (stream-rest s) (stream-rest t))
    )
  )
)

(define (make-integers)
  (define (make-integers-inner i)
    (stream-cons i (make-integers-inner (+ i 1)))
  )
  (make-integers-inner 1)
)

(define integers (make-integers))

(define integer-pairs (pairs integers integers))

(display "on ") (display 0) (display " = ") (stream-ref integer-pairs 0) 
(display "on ") (display 1) (display " = ") (stream-ref integer-pairs 1) 
(display "on ") (display 2) (display " = ") (stream-ref integer-pairs 2) 
(display "on ") (display 3) (display " = ") (stream-ref integer-pairs 3) 
(display "on ") (display 4) (display " = ") (stream-ref integer-pairs 4) 
(display "on ") (display 5) (display " = ") (stream-ref integer-pairs 5) 
(display "on ") (display 6) (display " = ") (stream-ref integer-pairs 6) 
(display "on ") (display 7) (display " = ") (stream-ref integer-pairs 7) 
(display "on ") (display 8) (display " = ") (stream-ref integer-pairs 8) 
(display "on ") (display 9) (display " = ") (stream-ref integer-pairs 9) 
(display "on ") (display 10) (display " = ") (stream-ref integer-pairs 10) 
(display "on ") (display 11) (display " = ") (stream-ref integer-pairs 11) 
(display "on ") (display 12) (display " = ") (stream-ref integer-pairs 12) 
(display "on ") (display 13) (display " = ") (stream-ref integer-pairs 13) 
(display "on ") (display 14) (display " = ") (stream-ref integer-pairs 14) 
(display "on ") (display 15) (display " = ") (stream-ref integer-pairs 15) 
(display "on ") (display 16) (display " = ") (stream-ref integer-pairs 16) 
(display "on ") (display 17) (display " = ") (stream-ref integer-pairs 17) 
(display "on ") (display 18) (display " = ") (stream-ref integer-pairs 18) 
(display "on ") (display 19) (display " = ") (stream-ref integer-pairs 19) 

; number increse on left, right, left, right, ...

; (1, 2) -> (1, 3) -> skip one each time -> (1, 100) = (100 - 2) * 2 + 1 = 197
; (display "on ") (display 197) (display " = ") (stream-ref integer-pairs 197)

; with some my-style annotations... whatever...
;
; Pair(i~, i~) = ((i, i), Interleave((i, i+1~), Pair(i+1~, i+1~)))
; 
; So 
; (i, i) is the first item of Pair(i~,i~)
; (i, i+1) is the second item of Pair(i~,i~)
; (i+1,i+1) is the third item of Pair(i~,i~)

; thanks to https://wqzhang.wordpress.com/2009/08/17/sicp-exercise-3-66/
; finally understand it

; (99, 100) => second element of (99~, 99~) => fourth element of Interleave((i, i+1~), Pair(i+1~, i+1~)) => fifth element of (98~, 98~)
; loop it!
; (((x * 2 + 1) * 2 + 1) * 2 + 1) ... ) where x the the first "second"
; = x * 2 ^ (n-1) + 2 ^ (n - 2) + 2 ^ (n - 3) + + 2 ^ (n - 4) + ... + 2 ^ 0
; = x * 2 ^ (n-1) + 2 ^ (n - 1) - 1

; for (99, 100), x = 2, n = 99 => 2 ^ 99 + 2 ^ 98 - 1 (-1 again for preceding element)
; for (100, 100), x = 1, n = 100 => 2 ^ 100 - 1　(-1 again for preceding element)

; -------

; another idea from a friend
; check sequence (i, i) and (i, i + 1) -> find out what that is

; 
