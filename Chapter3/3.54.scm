#lang racket
(require racket/stream)

(define (add-streams s1 s2) (stream-cons (+ (stream-first s1) (stream-first s2)) (add-streams (stream-rest s1) (stream-rest s2))))
(define (mul-streams s1 s2) (stream-cons (* (stream-first s1) (stream-first s2)) (mul-streams (stream-rest s1) (stream-rest s2))))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(define factorials (stream-cons 1 (mul-streams factorials integers)))

(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
(stream-ref factorials 5)
(stream-ref factorials 6)