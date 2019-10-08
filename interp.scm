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


(define true
  '(lambda (x) (lambda (y) x)))
(define false
  '(lambda (x) (lambda (y) y)))

(define-syntax iif
  (syntax-rules (then else)
    ([_ p then e1 else e2] (_if p e1 e2))))
(define (_if p x y)
  `((,p ,x) ,y))

(define (iand p1 p2)
  `((,p1 ,p2) ,false))
(define (ior p1 p2)
  `((,p1 ,true) ,p2))

(define (inot p)
  `((,p ,false) ,true))


(define-syntax ilet
  (syntax-rules (be in)
    ([_ x be y in body] (_let x y body))))
(define (_let x y body)
  `((lambda (,x) ,body) ,y))

(define (repeat n)
  (cond
    [(= n 0) 'x]
    [else `(f ,(repeat (sub1 n)))]))

(define (number n)
  `(lambda (f) (lambda (x) ,(repeat n))))

(define (add x y)
  `(lambda (f) (lambda (x) ((,x f) ((,y f) x)))))

(define (print-number n)
  (((eval-expr n empty) add1) 0))
