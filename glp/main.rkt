#lang racket
(provide (rename-out [glp-topi #%top-interaction]
                     [glp-module #%module-begin]
                     [glp-lambda lambda]
                     [glp-app #%app]
                     [glp-top #%top]
                     [glp-define define]
                     [glp-traces traces]
                     [glp-stepper stepper]
                     [glp-type type]
                     [glp-normalize normalize])
         Set
         ->
         ::
         ?
         nfx
         trace-on
         trace-off
         glp-app)


(require (for-syntax syntax/parse))
 (require (for-syntax racket/syntax))
(require (only-in redex/reduction-semantics
                  judgment-holds
                  term
                  apply-reduction-relation*))
(require redex)
(require (only-in "lang_simple.rkt"
                  GradualElabSynth
                  GradualNormSynth
                  GradualSynth
                  red
                  L))

(require typeset-rewriter)

(begin-for-syntax
  (define-syntax-class arrow-dom
    #:datum-literals (:)
    (pattern (var:id : ty:expr))
    (pattern ty:expr
             #:attr var (generate-temporary 'x)         )
    )
  )


(define-syntax (glp-top stx)
  (syntax-parse stx
    [(_ . x)
     #`(term x)]))

(define-syntax (glp-lambda stx)
  (syntax-parse stx
    [(_ x:id body:expr)
     #`(term (TermLam x (unquote body)))]
    [(_ (x:id ...) body:expr)
     (foldr (lambda (arg funBody)
              #`(term (TermLam (unquote #,arg) (unquote #,funBody )))) 
            #'body
            (syntax->list #'(x ...) ))]
    ))

(define-syntax (glp-app stx)
  (syntax-parse stx
    [(_ s:expr t:expr ...)
     (foldl
      (lambda (arg fn)
        #`(term (TermApp (unquote #,fn) (unquote #,arg))))
      #'s
      (syntax->list #'(t ...) ))
     ]))

(define-syntax (Set stx)
  (syntax-parse stx
    [(_ i:nat)
     #`(term (TermSet i))]))

(define ? (term TermDyn))

(define-syntax (:: stx)
  (syntax-parse stx
    [(_ s:expr t:expr)
     #`(term (TermAnn (unquote s) (unquote t)))]))

(define-syntax (-> stx)
  (syntax-parse stx
    [(_ dom:arrow-dom ...+ t:expr )
     (foldr
      (lambda (xarg sarg cod)
     #`(term (TermPi #,xarg (unquote #,sarg) (unquote #,cod)))
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
                  [(< 1 (length elabList)) (error "Too many elaborations for expression" e elabList) ]
                  [else (first elabList)]
                  )))

(define (typecheck e)
  (let ([tlist (judgment-holds (GradualSynth EnvEmpty (unquote e) gU) gU)])
           (cond
                  [(null? tlist) (error "Can't infer type for expression (try adding an annotation?)" e )]
                  [(< 1 (length tlist)) (error "Too many types for expression" e tlist) ]
                  [else (first tlist)]
                  )))

(define (norm-and-typecheck e)
  (let ([normList (judgment-holds (GradualNormSynth EnvEmpty (unquote e) gu gU) gu)])
           (cond
                  [(null? normList) (error "Can't infer type to normalize expression (try adding an annotation?)" e )]
                  [(< 1 (length normList)) (error "Too many normalizations for expression" e normList) ]
                  [else (first normList)]
                  )))

(define-for-syntax (eval-rdx e)
  (syntax-parse e
    #:literals (glp-define trace-on trace-off glp-traces glp-stepper)
    [(glp-define x body)
     #'(glp-define x body)]
    [(trace-on)
     #'(trace-on)]
    [(trace-off)
     #'(trace-off)]
    [(glp-traces body)
     #'(glp-traces body)]
    [(glp-stepper body)
     #'(glp-stepper body)]
    [_
    #`(apply values
                (apply-reduction-relation* red (elab-and-typecheck #,e)))
    ]
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
      #`(define x (begin  (typecheck body)  body)
      )]
    ;[(_ (f:id arg:id ...) body)
    ;  #`(define f (glp-lambda (arg ...) body)
    ;  )]
    [(_ (f1:id : tp:expr) (f2:id arg:id ... = body))
     #:fail-unless (equal? (syntax-e #'f1) (syntax-e #'f2)) "Must give definition to match type declaration"
      #`(glp-define f1 (:: (glp-lambda (arg ...) body) tp)
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

(define-syntax (glp-normalize stx)
  (syntax-parse stx
    [(_ body)
      #`(norm-and-typecheck body
      )]))

(define-syntax (trace-on stx)
  #'(current-traced-metafunctions 'all))

(define-syntax (trace-off stx)
  #'(current-traced-metafunctions '()))

(define-syntax (nfx stx)
  (syntax-parse stx
    ; #:datum-literals (: )
    #:literals (->)
  [(nfx dom:arrow-dom ...+ -> cod:expr) #'(-> dom ... cod)]
  ))

(define lambda-rw
  (rw-lambda
   [`(TermLam ,x ,body) => (list "λ" x ". " body)
    ]))

(define-rw-context with-glp-rewrites
  #:atomic (['TermDyn "?"])
  #:compound (['TermLam lambda-rw]))