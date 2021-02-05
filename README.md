Metacircular-Evaluator
The Scheme interpreter in SICP[1] which is implemented by Scheme


> The evaluator, which determines the meaning of expressions in a programming language, is just another program.

## How to use this scheme interpreter
step1. Load the interpreter

      (load "evaluator.scm")

step2. enter read-eval-print loop

      (driver-loop)
      
step3. feed in some expressions


      ;;; M-Eval input:
      (define (append x y)
        (if (null? x)
            y
            (cons (car x) (append (cdr x) y))))
            
      ;;; M-Eval value:
      ok

      ;;; M-Eval input:
      (append '(a b c) '(d e f))

      ;;; M-Eval value:
      (a b c d e f)
[1] http://sarabander.github.io/sicp/html/index.xhtml#SEC_Contents
