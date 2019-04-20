#lang racket
(provide (rename-out [GDTL-topi #%top-interaction]
                     [GDTL-module #%module-begin]
                     [GDTL-lambda lambda]
                     [GDTL-app #%app]
                     [GDTL-top #%top]
                     [GDTL-define define]
                     [GDTL-traces traces]
                     [GDTL-stepper stepper]
                     [GDTL-type type]
                     [GDTL-normalize normalize]
                     [GDTL-datum #%datum])
         Set
         ->
         ::
         ?
         nfx
         trace-on
         trace-off
         reductions
         Nat
         Zero
         Succ
         NatElim
         Vec
         Nil
         Cons
         VecElim)


(require (for-syntax syntax/parse))
 (require (for-syntax racket/syntax))
(require (only-in redex/reduction-semantics
                  judgment-holds
                  term
                  apply-reduction-relation*))
(require redex)
(require  (rename-in "lang_simple.rkt" (currently-defined-vars defined-vars )) )

(default-language LContext)

;(check-redundancy #t)
(caching-enabled? #t)
(set-cache-size! 10000)
(current-cache-all? #t)

(begin-for-syntax
  (define-syntax-class arrow-dom
    #:datum-literals (:)
    (pattern (var:id : ty:expr))
    (pattern ty:expr
             #:attr var (generate-temporary 'x)         )
    )
  )

;(define defined-vars (make-hasheq))


(define-syntax (GDTL-top stx)
  (syntax-parse stx
    [(_ . x)
     #`(term x)]))

(define-syntax (GDTL-lambda stx)
  (syntax-parse stx
    [(_ x:id body:expr)
     #`(term (TermLam x (unquote body)))]
    [(_ (x:id ...) body:expr)
     (foldr (lambda (arg funBody)
              #`(term (TermLam (unquote #,arg) (unquote #,funBody )))) 
            #'body
            (syntax->list #'(x ...) ))]
    ))

(define-syntax (GDTL-app stx)
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

(define Nat (term TermNat))
(define Zero (term TermZero))
(define-syntax (Succ so)
  (syntax-case so ()
      ((_ gu ) #'(term (TermSucc ,gu))
       )))
(define-syntax (NatElim so)
  (syntax-case so ()
      ((_ gu gU gu_Z x_n x_rec gu_S) #'(term (TermNatElim 1 ,gu ,gU ,gu_Z ,x_n ,x_rec ,gu_S))
       )
    ((_ i gu gU gu_Z x_n x_rec gu_S) #'(term (TermNatElim i ,gu ,gU ,gu_Z ,x_n ,x_rec ,gu_S))
       )
    ))

(define-syntax (Vec so)
  (syntax-case so ()
      ((_ gU gu ) #'(term (TermVec ,gU ,gu))
       )))
(define-syntax (Nil so)
  (syntax-case so ()
      ((_ gu ) #'(term (TermNil ,gu))
       )))
(define-syntax (Cons so)
  (syntax-case so ()
      ((_ tp len hd tail ) #'(term (TermCons ,tp ,len ,hd ,tail))
       )))
(define-syntax (VecElim so)
  (syntax-case so ()
      ((_ vec tp len motive n x1 x2 x3 x4 c) #'(term (TermVecElim 1 ,vec ,tp ,len ,motive ,n ,x1 ,x2 ,x3 ,x4 ,c))
       )
    ((_ i vec tp len motive n x1 x2 x3 x4 c) #'(term (TermVecElim i ,vec ,tp ,len ,motive ,n ,x1 ,x2 ,x3 ,x4 ,c))
       )))


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

(define MultiStep (context-closure Red LContext redexContext))



(define (elab-and-typecheck e)
  (with-handlers ([(const #t) (lambda (exn) (error (format "Static Type Error checking ~s:\n~a\n" (pt e) exn) (current-error-port)) )])
      (let ([elabList (judgment-holds (GElabSynth EnvEmpty (unquote e) et gU) et)])
           (cond
                  [(null? elabList) (error "Can't infer type for expression (try adding an annotation?)" e )]
                  [(< 1 (length elabList)) (error "Too many elaborations for expression" e elabList) ]
                  [else (first elabList)]
                  ))))

(define (typecheck e)
  (with-handlers ([(const #t) (lambda (exn) (error (format "Static Type Error checking ~s:\n~a\n" (pt e) exn) (current-error-port)) )])
   (let ([tlist (judgment-holds (ElabNormType EnvEmpty (unquote e) ent) ent)])
           (cond
                  [(null? tlist) (error "Can't infer type for definition (try adding an annotation?)" e )]
                  [(< 1 (length tlist)) (error "Too many types for expression" e tlist) ]
                  [else (first tlist)]
                  ))))

(define (norm-and-typecheck e)
  (with-handlers ([(const #t) (lambda (exn) (error (format "Static Type Error checking ~s:\n~a\n" (pt e) exn) (current-error-port)) )])
   (let ([normList (judgment-holds (GNSynth EnvEmpty (unquote e) gu gU) gu)])
           (cond
                  [(null? normList) (error "Can't infer type to normalize expression (try adding an annotation?)" e )]
                  [(< 1 (length normList)) (error "Too many normalizations for expression" e normList) ]
                  [else (first normList)]
                  ))))

(define-for-syntax (eval-rdx e)
  (syntax-parse e
    #:literals (GDTL-define trace-on trace-off trace-list GDTL-traces GDTL-stepper)
    [(GDTL-define x body)
     #'(GDTL-define x body)]
    [(trace-on)
     #'(trace-on)]
    [(trace-off)
     #'(trace-off)]
    [(trace-list l)
     #'(trace-list l)]
    [(GDTL-traces body)
     #'(GDTL-traces body)]
    [(GDTL-stepper body)
     #'(GDTL-stepper body)]
 ;   [(reductions body)
  ;   #'(reductions body)]
    [_
    #`(with-handlers ([(const #t) (lambda (exn) (displayln (format "Static Type Error checking ~s:\n~a\n" (pt #,e) exn) (current-error-port)) )])(let ([elabbed (perform-elab-substs (elab-and-typecheck #,e))])
       (with-handlers ([(const #t) (lambda (exn) (displayln (format "Dynamic Type Error running ~s:\n~a\n" (pt #,e) exn) (current-error-port)) )])
         (apply values
                (map pt (apply-reduction-relation* MultiStep elabbed))))))
    ]
  )
)


(define-syntax (GDTL-topi stx)
  (syntax-parse stx
    [(_ . e)
     (eval-rdx #'e)
     ]
    ))



(define-syntax (GDTL-module stx)
  (syntax-parse stx
    [(_ e ...)
     #:do [
     [define elist (syntax->list #'(e ...)) ]
     [define explist (map eval-rdx elist)]
     ]
      #`(#%module-begin  #,@explist
      )]))

(define-syntax (GDTL-define stx)
  (syntax-parse stx
    #:datum-literals (:=)
    [(_ x:id body)
      #`(let*
              ([key '#,(syntax->datum #'x)]
               [ent (typecheck body)])
                    (printf  "defined ~a\n" (quote x))  (hash-set! defined-vars key ent))
      ]
    ;[(_ (f:id arg:id ...) body)
    ;  #`(define f (GDTL-lambda (arg ...) body)
    ;  )]
    [(_ (f1:id : tp:expr) (f2:id  = body))
     #:fail-unless (equal? (syntax-e #'f1) (syntax-e #'f2)) "Must give definition to match type declaration"
      #`(GDTL-define f1 (:: body tp)
      )]
    [(_ (f1:id : tp:expr) (f2:id arg:id ...+ = body))
     #:fail-unless (equal? (syntax-e #'f1) (syntax-e #'f2)) "Must give definition to match type declaration"
      #`(GDTL-define f1 (:: (GDTL-lambda (arg ...) body) tp)
      )]
    ))

(define-syntax (GDTL-traces stx)
  (syntax-parse stx
    [(_ body)
      #`(traces MultiStep (perform-elab-substs (elab-and-typecheck body)) #:pp pt
      )]))

(define-syntax (reductions stx)
  (syntax-parse stx
    [(_ body)
      #`(apply-reduction-relation/tag-with-names MultiStep (perform-elab-substs (elab-and-typecheck body))
      )]))


(define-syntax (GDTL-stepper stx)
  (syntax-parse stx
    [(_ body)
      #`(stepper MultiStep (perform-elab-substs (elab-and-typecheck body)) pt
      )]))

(define-syntax (GDTL-type stx)
  (syntax-parse stx
    [(_ body)
      #`(show-derivations (build-derivations (GradualElabSynth EnvEmpty (unquote body) es gU))
      )]))

(define-syntax (GDTL-normalize stx)
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

(define-for-syntax (number->term n)
  (if (= n 0)
      #`TermZero
      #`(TermSucc #,(number->term (- n 1))) ))

(define-syntax (GDTL-datum stx)
  (syntax-parse stx
    [(_ . v:exact-nonnegative-integer) #`(term #,(number->term (syntax->datum #`v)))]
    [(_ . other) (raise-syntax-error #f "not allowed" #'other)]))

