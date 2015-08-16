#lang scheme

(define (make-account balance password)
  (define pwd password)
  (define (withdraw amount)

    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (cond ((not (eq? password pwd)) 
    		(lambda (x . y) (print "Incorrect password") (newline)))
    	  ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'withdraw) 50)