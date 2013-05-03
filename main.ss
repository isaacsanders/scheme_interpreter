; <form>                         ::= <definition>
;                                  | <expression>
; <definition>                   ::= <variable definition>
;                                  | (begin <definition>*)
; <variable definition>          ::= (define <variable> <expression>)
; <expression>                   ::= <constant>
;                                  | <variable>
;                                  | (quote <datum>)
;                                  | (lambda <formals> <expression> <expression>*)
;                                  | (if <expression> <expression> <expression>)
;								   | (set! <variable> <expression>)
;								   | (let ((<variable> <expression>)*) <expression>*)
;								   | (let <variable> ((<variable> <expression>)*) <expression>*)
;								   | (let* ((<variable> <expression>)*) <expression>*)
;								   | (letrec ((<variable> <expression>)*) <expression>*)
;								   | (cond ((<expression> <expression>*)*))
;								   | (and <expression>*)
;								   | (or <expression>*)
;								   | (case <expression> ((<variable> <expression>*)*))
;								   | (while <expression> <expression>*)
;                                  | <application>
; <variable>                     ::= <lexically-addressed-variable>
;                                  | <free-variable>
; <lexically-addressed-variable> ::= (<symbol> : <number> <number>)
; <free-variable>                ::= (<symbol> free)
; <constant>                     ::= <boolean> | <number> | <character> | <string>
; <formals>                      ::= <variable>
;                                  | (<variable>*)
;                                  | (<variable> <variable>* . <variable>)
; <application>                  ::= (<expression> <expression>*)

(load "interpreter.ss")
(load "functional-utils.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (write (eval-expression
             (lexical-address
               (syntax-expand
                 (parse-top-expression
                   (read))))
             (lexically-addressed-environment (list))))
    (newline)
    (rep)))

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
