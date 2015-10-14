#lang scheme

(require scheme/mpair)

; skip implementation..

; as doubly linked list

(define loop (mcons 1 (mcons 2 3)))
loop
(set-mcdr! loop (cons loop loop))
loop

; mpair handled loop...

; or use a seperated list to store reverse links