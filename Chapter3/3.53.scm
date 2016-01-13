#lang racket
(require racket/stream)

(define (integers-starting-from n)
	(stream-cons n (integers-starting-from (+ n 1)))
)

(stream-ref (integers-starting-from 5) 10000)
; (stream->list (integers-starting-from 1)) ; infinity


;(define (add-streams s1 s2) (stream-map + s1 s2)) ; stream-map not enhanced...

; make a simple replacement but not deal with empty stream
(define (add-streams s1 s2) (stream-cons (+ (stream-first s1) (stream-first s2)) (add-streams (stream-rest s1) (stream-rest s2))))
(define s (stream-cons 1 (add-streams s s))) ; should give 1 2 4 8 16 ... 2^n
(stream-ref s 4)
(stream-ref s 5)