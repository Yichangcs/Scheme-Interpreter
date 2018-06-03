;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Eval and Analyze               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
   (cond ((self-evaluating? exp)
           (analyze-self-evaluating exp))
         ((variable? exp)
          (analyze-variable exp))
         ((quoted? exp)
          (analyze-quoted exp))
         ((assignment? exp)
          (analyze-assignment exp))
         ((definition? exp)
          (analyze-definition exp))
         ((if? exp)
          (analyze-if exp))
         ((lambda? exp) 
           (analyze-lambda exp))
         ((begin? exp)
          (analyze-sequence
            (begin-actions exp)))
         ((cond? exp)
           (analyze (cond->if exp)))
         ((application? exp) 
           (analyze-application exp))
           (else
              (error "Unknown expression
                      type: EVAL" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 Application             ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application 
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error "Unknown procedure type: 
                      EXECUTE-APPLICATION"
                     proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Conditions               ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 lambda                  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence 
                (lambda-body exp))))
    (lambda (env) 
      (make-procedure vars bproc env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 Sequence                ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))               ;; procs in rest-procs are extracted 
  (let ((procs (map analyze exps)))             ;; into a nested lambda expression one
    (if (null? procs)                           ;; by one
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Assignments and definitions       ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env)
      (set-variable-value! 
       var (vproc env) env)
      'ok)))


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze 
                (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           analyze other things           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (analyze-self-evaluating exp)
  (lambda (env) exp))  ;; ignores its environment argument

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) 
    (lookup-variable-value exp env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Representing Expressions       ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (self-evaluating? exp)
   (cond ((number? exp)  true)
         ((string? exp)  true)
         (else  false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
   (tagged-list? exp 'quote))

 (define (text-of-quotation exp)
    (cadr exp))

 (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp)  tag)
        false))

(define (assignment? exp)
   (tagged-list? exp 'set!))

(define (assignment-variable exp)
   (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
   (tagged-list? exp 'define))
(define (definition-variable exp)
   (if (symbol? (cadr exp))    ;; (define <var> <value>)
       (cadr exp)
       (caadr exp)))           ;; another form: (define (<var> <param1> ... <parami>) <body>)
(define (definition-value exp)
   (if (symbol? (cadr exp))
       (caddr exp)
        (make-lambda           ;; another form above
          (cdadr exp)   ;; <formal parameters>
          (cddr exp)))) ;； <body>    

(define (lambda? exp)
   (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
   (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
   (if (not (null? (cdddr exp)))
       (cadddr exp)
        'false))
(define (make-if predicate
                 consequent 
                 alternative)
   (list 'if
         predicate
         consequent
         alternative))

(define (begin? exp)
   (tagged-list? exp 'begin))
(define (begin-actions exp)(cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
   (cond ((null? seq) seq)
         ((last-exp? seq) (first-exp seq))   ;; just one expression
         (else (make-begin seq))))           ;; more than one expression
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))   ;; compound expression not one of the above types
(define (operator exp) (car exp))
(define (operands exp)(cdr exp))          ;; a list of operands
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        logical constants              ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define true #t)
(define false #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Derived expression          ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cond? exp)
   (tagged-list? exp 'cond))

(define (cond-clauses exp)  (cdr exp))

(define (cond-else-clause? clause)                    ;; (cond ((> x 0) x)
   (eq? (cond-predicate clause) 'else))               ;;       ((= x 0) (display 'zero) 0)
                                                      ;;       (else (- x)))
(define (cond-predicate clause) (car clause))  

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
   (if (null? clauses)
       'false  ;no else clause
       (let ((first (car clauses))
             (rest  (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp
                     (cond-actions first))
                  (error "ELSE clause isn't 
                         last: COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (sequence->exp
                          (cond-actions first))
                       (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Evaluator Data Structures         ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing of predicates
 (define (true? x) (not (eq? x false)))
 (define (fasle? x) (eq? x false))

;; Compound procedures
(define (make-procedure parameters body env)     ;; used in eval when processing lambda expression
   (list 'procedure parameters body env))

(define (compound-procedure? p) (tagged-list? p 'procedure))  

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Operations on Environment         ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an environment is a sequence of frames, where each frame is a
;; table of bindings that associate variables with their corresponding values.

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
   (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame)(cdr frame))

(define (add-binding-to-frame! var val frame)
   (set-car! frame (cons var (car frame)))
   (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)  ;; return a new environment, consisting of 
   (if (= (length vars) (length vals))           ;; a new frame in which the symbols in the list
       (cons (make-frame vars vals) base-env)    ;; <vars> are bound to the corresponding <values>
       (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)    ;; return the value that is bound to 
   (define (env-loop env)                  ;; the symbol <var> in the environment <env>
      (define (scan vars  vals)
         (cond ((null? vars)
               (env-loop
                  (enclosing-environment env)))
               ((eq? var (car vars)) (car vals))
               (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
               (scan (frame-variables frame)
                     (frame-values frame)))))
    (env-loop  env))

(define (set-variable-value! var val env)   ;; changes the binding of the <var> in the <env> so that
   (define (env-loop env)                   ;; the variable is now bound to the <value>
      (define (scan vars vals)
         (cond ((null? vars)
                (env-loop
                   (enclosing-environment env)))
               ((eq? var (car vars)) (set-car! vals var))
               (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
         (scan (frame-variables  frame)
               (frame-values frame)))))
 (env-loop env))

(define (define-variable! var val env)   ;; adds to the first frame in the environment <env> a new
   (let ((frame (first-frame env)))      ;; binding that associates the variable <var> with the <value>
      (define (scan vars vals)
         (cond ((null? vars)
               (add-binding-to-frame! var val frame))
               ((eq? var (car vars)) (set-car! vals val))
               (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame) (frame-values frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Running the Evaluator as a Program    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define primitive-procedures 
   (list (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list '+  +)
         (list '-  -)
         (list '*  *)
         (list '/  /)
         (list '=  =)
         ;; more primitives
         ))

(define (primitive-procedure-objects)   
   (map (lambda (proc)
           (list 'primitive  (cadr proc)))   ;; which results in ('primitive car) 
         primitive-procedures))

(define (primitive-procedure-names)
   (map car primitive-procedures))

(define (setup-environment)
   (let ((initial-env
           (extend-environment
              (primitive-procedure-names)    ;; carry out a procedure to produce names
              (primitive-procedure-objects)  ;; carry out a procedure to produce objects
              the-empty-environment)))
      (define-variable! 'true true initial-env)
      (define-variable! 'false false initial-env)
      initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)   ;; a primitive procedures is represented as 
   (tagged-list? proc 'primitive))    ;; a list beginning with "primitive" and containing a procedure
                                      ;; in the underlying Lisp that implements that primitive
(define (primitive-implementation proc)
   (cadr proc))

(define meta-apply-in-underlying-schme apply) ;; to apply primitive procedures

(define (meta-apply-primitive-procedure proc args)
   (meta-apply-in-underlying-schme
     (primitive-implementation proc) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Read-eval-print-loop           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
     (let ((output
             (eval input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
   (newline) (newline)
   (display string) (newline))

(define (announce-output string)
   (newline) (display string) (newline))

(define (user-print object)
   (if (compound-procedure? object)
       (display
         (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>))
      (display object)))
