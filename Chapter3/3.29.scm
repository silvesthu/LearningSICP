#lang scheme

; A or B = not ((not A) and (not B))

; need 3 more wires for intermediate result

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (not-a1-and-not-a2 (make-wire)))
      (inverter a1 not-a1)
      (inverter a2 not-a2)
      (and-gate not-a1 not-a2 not-a1-and-not-a2)
      (inverter not-a1-and-not-a2 output)))
  (add-action! a1 or-action-procedure) ; add action on each input
  (add-action! a2 or-action-procedure)
  'ok)