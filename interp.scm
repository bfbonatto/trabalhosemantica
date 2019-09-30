;; Although we don't need it, pattern-matching is very useful
(load "pmatch.scm")

;; the star of the show: the interpreter
;; this function interprets a lambda term using
;; an environment defined, in the usual way, as a
;; function that partially maps a symbol to a value
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
;; forall x. (empty x) -> error
(define empty
  (lambda (y) (error 'lookup (string-append "unbound " (symbol->string y)))))

;; helper function, just extends the current environment
;; forall x. forall e. forall value. ((extend e x v) x) = v
(define (extend old e new)
  (lambda (y)
    (if (eq? y e) new (old y))))

;; helper function, checks if the procedure returned by eval-expr
;; is (true) or (false)
(define (is-true? p) (= 1 ((p 1) 2)))

;; macro for translating multi argument functions into
;; one argument functions
(define-syntax ilambda
  (syntax-rules ()
    [(_ (x) body) `(lambda (x) ,(expand body))]
    [(_ (x y ...) body) `(lambda (x) ,(expand ( ilambda (y ...) body )))]))

;; church encoded true and false
(define-syntax true
  (syntax-rules ()
    [(_) (ilambda (x y) 'x)]))
(define-syntax false
  (syntax-rules ()
    [(_) (ilambda (x y) 'y)]))

;; church encoded if
(define-syntax iif
  (syntax-rules (ithen ielse)
    [(_ p ithen e1 ielse e2) `((,p ,e1) ,e2)]))

;; let encoded as function application
(define-syntax ilet
  (syntax-rules (be in)
    [(_ x be y in body) `(,(ilambda (x) body) ,y)]))

(define-syntax iand
  (syntax-rules ()
    [(_ x y) `((,x ,y) ,(false))]))
(define-syntax ior
  (syntax-rules ()
    [(_ x y) `((,x ,(true)) ,y)]))
