(load "parser.ss")

(define-datatype continuation continuation?
                 (halt-cont)
                 (rep-cont)
                 (if-cont
                   (true-exp expression?)
                   (cont continuation?)
                   (env list?))
                 (if-else-cont
                   (true-exp expression?)
                   (false-exp expression?)
                   (cont continuation?)
                   (env list?))
                 (eval-expressions-cont
                   (exps (list-of expression?))
                   (env scheme-value?)
                   (cont continuation?))
                 (cons-cont
                   (value scheme-value?)
                   (cont continuation?))
                 (proc-cont
                   (cont continuation?))
                 (begin-cont
                   (expr expression?)
                   (env scheme-value?)
                   (cont continuation?))
				 (while-cont
				   (test-exp expression?)
				   (bodies (list-of expression?))
				   (env scheme-value?)
				   (cont continuation?))
				 (define-cont
				   (sym symbol?)
				   (cont continuation?))
				 (set-local-cont
				   (depth number?)
				   (position number?)
				   (env scheme-value?)
				   (cont continuation?)))


(define scheme-value?
  (lambda (thing) #t))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           [halt-cont ()
                      val]
           [rep-cont ()
                     (pretty-print val)
                     (rep)]
           [cons-cont (value cont)
                      (apply-cont cont (cons value val))]
           [begin-cont (expr env cont)
                       (eval-expression expr cont env)]
           [proc-cont (cont)
                      (apply-proc (car val) (cdr val) cont)]
           [eval-expressions-cont (exps env cont)
                                  (eval-expressions exps (cons-cont val cont) env)]
           [if-cont (if-true-exp next-cont env)
                    (if val
                      (eval-expression if-true-exp next-cont env))]
           [if-else-cont (if-true-exp if-false-exp next-cont env)
                         (let [[expr (if val if-true-exp if-false-exp)]]
                           (eval-expression expr next-cont env))]
		   [while-cont (test-exp bodies env cont)
					(if val
						(eval-expression (begin-exp bodies) (while-cont test-exp bodies env cont) env)
						(apply-cont cont (void)))]
		   [define-cont (sym cont)
					(set! *global-env* (cons (cons sym val) *global-env*))
					(apply-cont cont (void))]
		   [set-local-cont (depth position env cont)
					(apply-cont cont (set-car! (list-tail
									(car
                                     (list-tail (cadr env) depth))
                                      position)
									   val))]
		   )))
