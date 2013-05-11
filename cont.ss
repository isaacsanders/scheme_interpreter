(load "parser.ss")
(load "interpreter.ss")

(define-datatype continuation continuation?
  (halt-cont)
  (if-cont
   (true-exp expression?)
   (cont continuation?)
   (env list?))
  (if-else-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?)))


(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   [halt-cont ()
		      (pretty-print val)]
	   [rep-cont ()
			  (pretty-print val)
			  (rep)]
	   [if-cont (if-true-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env))]
	   [if-else-cont (if-true-exp if-false-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env)
			(eval-expression if-false-exp next-cont env))])))