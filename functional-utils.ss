(define compose
  (lambda ls
    (letrec
      ((C (lambda (ls accu)
            (cond
              ((null? ls) (lambda x (apply accu x)))
              (else (lambda x ((car ls) (apply (apply compose (cdr ls)) x))))))))
      (C ls (lambda (a . b)
              (cond
                ((null? b) a)
                (else (cons a b))))))))

(define partial
  (lambda (proc . partially-applied-args)
    (lambda args
      (apply proc (append partially-applied-args args)))))
