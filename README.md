# Metacircular-Evaluator
A MIT-Scheme evaluator implemented by Scheme


> The evaluator, which determines the meaning of expressions in a programming language, is just another program.


## solving the definition mask issue of procedure apply
Having two different things called apply leads to a technical problem in running the metacircular evaluator, because defining the metacircular evaluator’s apply will mask the definition of the primitive.Here we have renamed the metacircular apply as meta-apply to avoid conflict with the name of the primitive procedure apply, which enable us to reference the underlying apply procedure in Scheme System by defining the apply-in-underlying-scheme procedure.

      (define apply-in-underlying-scheme apply)

## seperating syntatic Analysis from execution
The procedure analyze takes only the expression. It performs the syntactic analysis and returns a new procedure, the execution procedure, which is actually nested lambda expressions,that encapsulates the work to be done in executing the analyzed expression. The execution procedure takes an environment as its argument and completes the evaluation. This saves work because analyze will be called only once on an expression, while the execution procedure may be called many times.

      (define (eval exp env) ((analyze exp) env))
