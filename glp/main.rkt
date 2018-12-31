#lang racket
(provide (rename-out [glp-top #%top-interaction]
                     [glp-module #%module-begin]
                     [glp-lambda lambda]
                     [glp-app #%app])
         Set
         ->)


(require (for-syntax syntax/parse))
(require redex/reduction-semantics)
(require "lang_simple.rkt")


(define-syntax (glp-top stx)
  (syntax-parse stx
    [(_ . e)
     #'(#%top-interaction .
         (apply values
                (apply-reduction-relation* red (term e))))]))



(define-syntax (glp-module stx)
  (syntax-parse stx
    [(_ e ...)
     #'(#%module-begin
         (apply values
                (append (apply-reduction-relation* red (term e))
                        ...)))]))

(define-syntax (glp-lambda stx)
  (syntax-parse stx
    [(_ x:id body:expr)
     #'(TermLam x body)]))

(define-syntax (glp-app stx)
  (syntax-parse stx
    [(_ s:expr t:expr)
     #'(TermApp s t)]))

(define-syntax (Set stx)
  (syntax-parse stx
    [(_ i:nat)
     #'(TermSet i)]))

(define-syntax (-> stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (x:id : s:expr) t:expr )
     #'(TermPi x s t)]))