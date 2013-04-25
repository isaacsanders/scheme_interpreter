(define-syntax return-first
	(syntax-rules ()
		[(_) '()]
		[(_ e) e]
		[(_ e1 e2 e3 ...)
			(let ([a e1])
				(begin e2 e3 ... a))]))
				
(define-syntax for
	(syntax-rules ()
		[(_ (e1 _ e2 _ e3) e4)
			(begin e1
				(letrec ([helper
					(lambda ()
						(if e2
							(begin e4 e3 (helper))))])
						(helper)))]))
					