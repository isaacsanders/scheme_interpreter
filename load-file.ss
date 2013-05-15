(load "chez-init.ss")

(define load-file
  (lambda (filename)
    (cond
      ((file-exists? filename)
       (read-and-eval-exprs-in-seq (open-input-file filename)))
      (else
        (eopl:error 'load-file "File ~s does not exist." filename)))))

(define read-and-eval-exprs-in-seq
  (lambda (file-port)
    (let loop [[expr (read file-port)]]
      (if (not (eof-object? expr))
        (begin (eval-one-exp expr)
               (loop (read file-port)))))))
