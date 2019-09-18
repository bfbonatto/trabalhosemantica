;;Although we don't need it, pattern-matching is very useful
(load "pmatch.scm")

;; the star of the show, the interpreter
;; this function interprets a lambda term using
;; an environment, defined, in the usual way, as a
;; fucntion that partially maps a symbol to a value
(define (eval-expr expr env)
  (pmatch expr
    [,x (guard (symbol? x)) (env x)]
    [(lambda (,x) ,body)
     (lambda (arg)
       (eval-expr body (extend env x arg)))]
    [(,rator ,rand)
     ((eval-expr rator env)
      (eval-expr rand env))]
    )
  )

;; the empty environment
(define empty
  (lambda (y) (error 'lookup "unbound")))

;; helper function, just extends the current environment
(define (extend old e new)
  (lambda (y)
    (if (eq? y e) new (old y))))
