#lang racket
(require racket/stream)

(define (add-streams s1 s2) (stream-cons (+ (stream-first s1) (stream-first s2)) (add-streams (stream-rest s1) (stream-rest s2))))
(define (mul-streams s1 s2) (stream-cons (* (stream-first s1) (stream-first s2)) (mul-streams (stream-rest s1) (stream-rest s2))))

(define (partial-sums s)
	(define first-copies (stream-cons (stream-first s) first-copies))
	(stream-cons (stream-first s) (add-streams first-copies (partial-sums (stream-rest s)))))

(define (log2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (log2-summands (+ n 1)))))
(define log2-stream
  (partial-sums (log2-summands 1)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  
        (s1 (stream-ref s 1))  
        (s2 (stream-ref s 2))) 
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))
(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-first
              (make-tableau transform s)))

(define ln2 (accelerated-sequence euler-transform log2-stream))

(stream-ref ln2 0)
(stream-ref ln2 1)
(stream-ref ln2 2)
(stream-ref ln2 3)
(stream-ref ln2 4)
(stream-ref ln2 5)
(stream-ref ln2 6)
(stream-ref ln2 7)

; how rapid ?