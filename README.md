# Metacircular-Evaluator
A MIT-Scheme evaluator implemented by Scheme


> The evaluator, which determines the meaning of expressions in a programming language, is just another program.


## solving the definition mask issue of procedure apply
Having two different things called apply leads to a technical problem in running the metacircular evaluator, because defining the metacircular evaluator’s apply will mask the definition of the primitive. One way around this is to rename the metacircular apply to avoid conflict with the name of the primitive procedure, which enable us to reference the underlying apply procedure in Scheme System.

      (define apply-in-underlying-scheme apply)
