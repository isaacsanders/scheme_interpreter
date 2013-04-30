(define compose
  (lambda (a b)
    (lambda (c)
      (a (b c)))))

(define partial
  (lambda (proc . partially-applied-args)
    (lambda args
      (apply proc (append partially-applied-args args)))))
