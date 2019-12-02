#lang racket
(require "abbrv.rkt"
         (prefix-in - racket/base)
         (for-syntax syntax/parse racket/syntax syntax/stx)
         (for-meta 2 racket/base syntax/parse))
(provide #%module-begin
         (rename-out [checked-app #%app] [checked-λ λ] [checked-→ →]))

;; pattern expanders (not in paper) (must be at file top)
(begin-for-syntax
  ;; a → type must contain the literal →_intrnl identifier
  (define-syntax ~→
    (pattern-expander
     (syntax-parser
       [(_ tin tout)
        #'(_ (~literal →_intrnl) tin tout)]
       [(_ tin (~and ooo (~literal ...)) tout)
        #'(_ (~literal →_intrnl) tin ooo tout)]))))

;; figure 3
(define-m (checked-app-v0 e_fn e_arg) ; v0
  #:with (~→ τ_in τ_out) (compute-τ #'e_fn)
  #:with τ_arg (compute-τ #'e_arg)
  #:when (τ= #'τ_arg #'τ_in)
  #:with e_fn- (erase-τ #'e_fn) 
  #:with e_arg- (erase-τ #'e_arg) 
  (add-τ #'(-#%app e_fn- e_arg-) #'τ_out))

;; figure 4
(begin-for-syntax
  (define (add-τ e τ) 
    (add-stx-prop e 'type τ))
  (define (get-τ e)
    (get-stx-prop e 'type))
  (define (compute-τ e)
    (get-τ (local-expand e 'expression null)))
  (define (erase-τ e)
    (local-expand e 'expression null))
  (define (comp+erase-τ e) ; get e's type, erase types
    (with-syntax* ([e- (local-expand e 'expression null)]
                   [τ (get-τ #'e-)])
      #'[e- τ]))
  (define (τ= τ1 τ2) (stx= τ1 τ2)))

;; figure 5
(define-m (checked-app-v1 e_fn e_arg) ; v1
  #:with [e_fn- (~→ τ_in τ_out)] (comp+erase-τ #'e_fn)
  #:with [e_arg- τ_arg] (comp+erase-τ #'e_arg)
  #:when (τ= #'τ_arg #'τ_in)
  (add-τ #'(-#%app e_fn- e_arg-) #'τ_out))

;; figure 6
(define →_intrnl (λ _ (ERR "cannot use types at runtime")))
(define-m (→-v0 τ_in τ_out) #'(→_intrnl τ_in τ_out))
(define-m (checked-λ-v0 [x (~datum :) τ_in] e) ; v0
  #:with [(x-) e- τ_out] (comp+erase-τ/ctx #'e #'([x τ_in]))
  (add-τ #'(-λ (x-) e-) #'(→ τ_in τ_out)))
