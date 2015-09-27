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

; delegate to original account
(define (make-joint account original-password new-password)
  (define org-acc account)
  (define org-pwd original-password)
  (define new-pwd new-password)
  (define (withdraw amount)
    ((org-acc original-password 'withdraw) amount))
  (define (deposit amount)
    ((org-acc original-password 'deposit) amount))
  (define (dispatch password m)
    (cond ((not (eq? password new-pwd)) 
    		(lambda (x . y) (print "Incorrect password") (newline)))
    	  ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define peter-acc (make-account '100 'abc))

((peter-acc 'abc 'withdraw) 10)

(define paul-acc
	(make-joint peter-acc 'abc 'rosebud))

((paul-acc 'rosebud 'withdraw) 20)

((peter-acc 'abc 'withdraw) 10)