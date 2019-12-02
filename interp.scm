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
;; forall x. forall e. forall v. ((extend e x v) x) = v
(define (extend old e new)
  (lambda (y)
    (if (eq? y e) new (old y))))


(define-syntax iif
  (syntax-rules (then else)
    ([_ p then e1 else e2] `((,p ,e1) ,e2))))

(define-syntax true
  (syntax-rules ()
    ([_] '(lambda (x) (lambda (y) x)))))

(define-syntax false
  (syntax-rules ()
    ([_] '(lambda (x) (lambda (y) y)))))


;;;; church encoded "and" & "or"
(define-syntax iand
  (syntax-rules ()
  ([_ x y] `((,x ,y) ,(false)))))

(define-syntax ior
  (syntax-rules ()
  ([_ x y] `((,x ,(true)) ,y))))
;; church encoded not
(define-syntax inot
  (syntax-rules ()
    ([_ x] `((,x ,(false)) ,(true)))))


;; let defined as funcion application, macro for ease of use
(define-syntax ilet
  (syntax-rules (be in)
    ([_ x be y in body] `((lambda (,x) ,body) ,y))))

