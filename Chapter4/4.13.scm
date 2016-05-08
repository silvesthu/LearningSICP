#lang scheme

; copy from header.scm

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; ↓↓↓↓↓↓↓↓

; remove from first frame only..

; otherwise the lines below will be confusing

(define (f)
  (define A foo)
  (funcA)
  (funcB)     ; there is no assurance that if funcB will make-unbound! A
  (display A)
)

(define (remove-binding-to-frame! var frame)
  (define (remove-binding-to-frame-inner! var vars vals prev-vars prev-vals)
    (cond
      ((null? vars) (error "Unbound variable -- REMOVE-BINDING-TO-FRAME!" var))
      ((eq? var (car vars))
        (if (null? prev-vars)
          (begin (set-car! frame '()) (set-cdr! frame '())) ; var is the last one
          (begin (set-cdr! prev-vars (cdr vars)) (set-cdr! prev-vals (cdr vals)))
        )
      )
      (else (remove-binding-to-frame-inner var (cdr vars) (cdr vals) vars vals)) ; loop should be merged with the one in scan but prev need to be saved
    )
  )
  (remove-binding-to-frame-inner! var (car frame) (cdr frame) '() '())
)

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "Unbound variable -- MAKE-UNBOUND!" var))
            ((eq? var (car vars))
             (remove-binding-to-frame var frame))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))