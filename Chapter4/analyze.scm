; from http://www-inst.eecs.berkeley.edu/~cs61a/sp09/library/code/ch4.scm

;; *start* of analyzing evaluator
;;; **SEE ALSO** ch4-analyzingmceval.scm (loadable/runnable evaluator)

(define keyword-table-analyze '())
(define (put-keyword-analyze op proc) (set! keyword-table-analyze (append keyword-table-analyze (list (list op proc)))))
(define (get-keyword-analyze exp) 
  (define (get-inner t)
    (cond 
      ((null? t) #f)
      ((tagged-list? exp (caar t)) (cadar t))
      (else (get-inner (cdr t)))
    )
  )
  (get-inner keyword-table-analyze)
)

(define (eval-analyze exp env)
  ((analyze exp) env))

(define (analyze exp)
  (display "analyze: ")(display exp)(newline)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))

        ((get-keyword-analyze exp) ((get-keyword-analyze exp) exp))

        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (display "execute-application")(newline)
  (display "  proc: ")(display proc)(newline)
  (display "  args: ")(display args)(newline)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;; *end* of analyzing evaluator

(define (let->lambda-parameters lets)
  (map car lets)
)

(define (let->arguments lets)
  (map cadr lets)
)

(define (let->combination exp)
  (let 
    (
      (let-keyword (car exp))
      (lets (cadr exp))
      (body (cddr exp))
    )
    (define proc (analyze (make-lambda (let->lambda-parameters lets) body)))
    (define args (map analyze (let->arguments lets)))
    (lambda (env)
      (execute-application (proc env) (map (lambda (arg) (arg env)) args))
    )
  )
)

(put-keyword-analyze 'let let->combination)
