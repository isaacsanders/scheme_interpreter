(load "parser.ss")
(load "interpreter.ss")

(define-datatype continuation continuation?
  (halt-cont)
  (if-cont
   (true-exp expression?)
   (cont continuation?)
   (env list?)))


(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   [halt-cont ()
		      (pretty-print val)]
	   [if-cont (if-true-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env))])))