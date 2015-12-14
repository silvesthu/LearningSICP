#lang scheme

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

; set-value! -> set-my-value! -> for-each-except -> no constraints...

; skip the graph...