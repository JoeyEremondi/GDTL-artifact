#lang racket
(provide (rename-out [glp-topi #%top-interaction]
                     [glp-module #%module-begin]
                     [glp-lambda lambda]
                     [glp-app #%app]
                     [glp-top #%top]
                     [glp-define define]
                     [glp-traces traces]
                     [glp-stepper stepper]
                     [glp-type type])
         Set
         ->
         ::
         ?
         nfx)


(require (for-syntax syntax/parse))
 (require (for-syntax racket/syntax))
(require redex/reduction-semantics)
(require redex/gui )
(require "lang_simple.rkt")

(begin-for-syntax
  (define-syntax-class arrow-dom
    #:datum-literals (:)
    (pattern (var:id : ty:expr))
    (pattern ty:expr
             #:attr var (generate-temporary 'x)         )
    )
  )

(define-for-syntax (lexpand e)
  (local-expand e 'expression #f))

(define-syntax (glp-top stx)
  (syntax-parse stx
    [(_ . x)
     #`(term x)]))

(define-syntax (glp-lambda stx)
  (syntax-parse stx
    [(_ x:id body:expr)
     #`(term (TermLam x #,(lexpand #'(unquote body))))]
    [(_ (x:id ...) body:expr)
     (foldr (lambda (arg funBody)
              #`(term (TermLam (unquote #,arg) (unquote #,(lexpand funBody) )))) 
            #'body
            (syntax->list #'(x ...) ))]
    ))

(define-syntax (glp-app stx)
  (syntax-parse stx
    [(_ s:expr t:expr ...)
     (foldr
      (lambda (fn arg)
        #`(term (TermApp (unquote #,(lexpand fn)) (unquote #,(lexpand arg)))))
      #'s
      (syntax->list #'(t ...) ))
     ]))

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
    [(_ dom:arrow-dom ...+ t:expr )
     (foldr
      (lambda (xarg sarg cod)
     #`(term (TermPi #,xarg (unquote #,(lexpand sarg)) (unquote #,(lexpand cod))))
        )
        #'t
        (attribute dom.var)
        (attribute dom.ty)
      )
        ]))

(define (elab-and-typecheck e)
  (let ([elabList (judgment-holds (GradualElabSynth EnvEmpty (unquote e) es gU) es)])
           (cond
                  [(null? elabList) (error "Can't infer type for expression (try adding an annotation?)" e )]
                  [(< 1 (length elabList)) (error "Too many elaborations for expression" e) ]
                  [else (first elabList)]
                  )))

(define-for-syntax (eval-rdx e)
  (syntax-parse e
    [(glp-define x body)
     #'(glp-define x body)]
    [(glp-traces body)
     #'(glp-traces body)]
    [(glp-stepper body)
     #'(glp-stepper body)]
    [_
   (let
      ([expanded (local-expand e 'expression #f)])
    #`(apply values
                (apply-reduction-relation* red (elab-and-typecheck #,expanded)))
    )]
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
      #`(#%module-begin  #,@explist
      )]))

(define-syntax (glp-define stx)
  (syntax-parse stx
    #:datum-literals (:=)
    [(_ x:id body)
      #`(define x body
      )]
    ;[(_ (f:id arg:id ...) body)
    ;  #`(define f (glp-lambda (arg ...) body)
    ;  )]
    [(_ (f1:id : tp:expr) (f2:id arg:id ... = body))
     #:fail-unless (equal? (syntax-e #'f1) (syntax-e #'f2)) "Must give definition to match type declaration"
      #`(define f1 (:: (glp-lambda (arg ...) body) tp)
      )]
    ))

(define-syntax (glp-traces stx)
  (syntax-parse stx
    [(_ body)
      #`(traces red (elab-and-typecheck body)
      )]))

(define-syntax (glp-stepper stx)
  (syntax-parse stx
    [(_ body)
      #`(stepper red (elab-and-typecheck body)
      )]))

(define-syntax (glp-type stx)
  (syntax-parse stx
    [(_ body)
      #`(show-derivations (build-derivations (GradualElabSynth EnvEmpty (unquote body) es gU))
      )]))

(define-syntax (nfx stx)
  (syntax-parse stx
    #:datum-literals (:)
  [(nfx dom:arrow-dom ...+ -> cod:expr) #'(-> dom ... cod)]
  ))