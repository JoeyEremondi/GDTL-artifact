#lang racket
(provide (rename-out [glp-topi #%top-interaction]
                     [glp-module #%module-begin]
                     [glp-lambda lambda]
                     [glp-app #%app]
                     [glp-top #%top])
         Set
         ->
         ::
         ?)


(require (for-syntax syntax/parse))
(require redex/reduction-semantics)
(require "lang_simple.rkt")

(define-for-syntax (lexpand e)
  (local-expand e 'expression #f))

(define-syntax (glp-top stx)
  (syntax-parse stx
    [(_ . x)
     #`(term x)]))

(define-syntax (glp-lambda stx)
  (syntax-parse stx
    [(_ x:id body:expr)
     #`(term (TermLam x #,(lexpand #'(unquote body))))]))

(define-syntax (glp-app stx)
  (syntax-parse stx
    [(_ s:expr t:expr)
     #`(term (TermApp #,(lexpand #'(unquote s)) #,(lexpand #'(unquote t))))]))

(define-syntax (Set stx)
  (syntax-parse stx
    [(_ i:nat)
     #`(term (TermSet i))]))

(define-syntax (? stx)
  (syntax-parse stx
    [(_)
     #`(term (TermDyn))]))

(define-syntax (:: stx)
  (syntax-parse stx
    [(_ s:expr t:expr)
     #`(term (TermAnn #,(lexpand #'(unquote s)) #,(lexpand #'(unquote t))))]))

(define-syntax (-> stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (x:id : s:expr) t:expr )
     #`(term (TermPi x #,(lexpand #'(unquote s)) #,(lexpand #'(unquote t))))]))

(define-for-syntax (eval-rdx e)
  (let
      ([expanded (local-expand e 'expression #f)])
    #`(let*
          ([elabList (judgment-holds (GradualElabSynth EnvEmpty (unquote #,expanded) es gU) es)]
           [elab (cond
                  [(null? elabList) (error "No type for expression" #,expanded )]
                  [(< 1 (length elabList)) (error "Too many elaborations for expression" #,expanded) ]
                  [else (first elabList)]
                  )])
       (apply values
                (apply-reduction-relation* red elab)))
    )
  )


(define-syntax (glp-topi stx)
  (syntax-parse stx
    [(_ . e)
     (eval-rdx #'e)
     ]
    ))



(define-syntax (glp-module stx)
  (syntax-parse stx
    [(_ e ...)
     #:do [
     [define elist (syntax->list #'(e ...)) ]
     [define explist (map eval-rdx elist)]
     ]
      #`(#%module-begin #,@explist
      )]))
     
