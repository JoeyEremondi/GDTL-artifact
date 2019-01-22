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
         trace-list
         reductions
         glp-app)


(require (for-syntax syntax/parse))
 (require (for-syntax racket/syntax))
(require (only-in redex/reduction-semantics
                  judgment-holds
                  term
                  apply-reduction-relation*))
(require redex)
(require  "lang_simple.rkt" )


;(check-redundancy #t)
(caching-enabled? #t)

(begin-for-syntax
  (define-syntax-class arrow-dom
    #:datum-literals (:)
    (pattern (var:id : ty:expr))
    (pattern ty:expr
             #:attr var (generate-temporary 'x)         )
    )
  )

(define defined-vars (make-hasheq))


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
    #:literals (glp-define trace-on trace-off trace-list glp-traces glp-stepper)
    [(glp-define x body)
     #'(glp-define x body)]
    [(trace-on)
     #'(trace-on)]
    [(trace-off)
     #'(trace-off)]
    [(trace-list l)
     #'(trace-list l)]
    [(glp-traces body)
     #'(glp-traces body)]
    [(glp-stepper body)
     #'(glp-stepper body)]
    [(reductions body)
     #'(reductions body)]
    [_
    #`(apply values
                (map pt (apply-reduction-relation* SmallStep (elab-and-typecheck #,e))))
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
      #`(define x (begin
                    (hash-set! defined-vars '#,(syntax->datum #'x) body)
                    (typecheck body)
                    (println (list "defined" (quote x)))  body)
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
      #`(traces SmallStep (elab-and-typecheck body) #:pp pt
      )]))

(define-syntax (reductions stx)
  (syntax-parse stx
    [(_ body)
      #`(apply-reduction-relation/tag-with-names SmallStep (elab-and-typecheck body)
      )]))


(define-syntax (glp-stepper stx)
  (syntax-parse stx
    [(_ body)
      #`(stepper SmallStep (elab-and-typecheck body) pt
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

(define-syntax (trace-list stx)
  (syntax-parse stx
    [(_ . (x:id ...)) #'(current-traced-metafunctions (list 'x ... ))]
  ))

(define-syntax (trace-off stx)
  #'(current-traced-metafunctions '()))

(define-syntax (nfx stx)
  (syntax-parse stx
    ; #:datum-literals (: )
    #:literals (->)
  [(nfx dom:arrow-dom ...+ -> cod:expr) #'(-> dom ... cod)]
  ))


(define (pt tm)
  (with-handlers ([exn:fail:redex?
                   (lambda (exn) (format "~a" tm))])
    (pretty-term tm)
;    (let ([natbody (hash-ref! defined-vars 'nat)])
;      (cond
;    [(and natbody (alpha-equivalent? (term (TermAnn ,natbody (TermSet 5))))) (term nat)]
;    [else (pretty-term tm)]))
    ))

(define pretty-term
    (term-match/single L
    [x (symbol->string (term x))]
    [(TermLam x tt) (string-append "(λ " (pt (term x)) " . " (pt (term tt)) ")")]
    [(TermPi x SS TT) (string-append "(( " (pt (term x)) " : " (pt (term SS)) ") -> " (pt (term TT)) ")")]
    [(TermSet i) (string-append "Set" (number->string (term i)))]
    [(TermApp ss tt) (string-append "(" (pt (term ss)) " " (pt (term tt)) ")")]
    [TermDyn "?"]
    [TermError "⊥"]
    [(TermAnn ss tt) (string-append (pt (term ss)) " :: " (pt (term tt)))]
    [(TermDynAnn gU) (string-append "?" (pt (term gU)))]
    [(TermEp (EvidencePair gU gV) tt) (string-append "〈" (pt (term gU)) ", " (pt (term gV)) "〉" (pt (term tt)))]
    [(CanonicalAtomic grr) (pt (term grr))]
    [(CanonicalLam x gu) (string-append "(λ " (pt (term x)) " . " (pt (term gu)) ")")]
    [(CanonicalPi x gU gV) (string-append "(( " (pt (term x)) " : " (pt (term gU)) ") -> " (pt (term gV)) ")")]
    [(AtomicSet i) (string-append "Set" (number->string (term i)))]
    [CanonicalDyn "?"]
    [(AtomicSpine x ge) (string-append "(" (pt (term x)) " " (pt (term ge)) ")")]
    [SpineEmpty ""]
    [(SpineCons ge gu) (string-append (pt (term ge)) " " (pt (term gu)))]
    
    ))

