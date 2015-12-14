#lang scheme

(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; 100 remains
; first set! calculated first, set last