#lang scribble/manual
@require[racket/date]
@require[(for-label GDTL/main)]
@require[(for-label (rename-in redex
                               [stepper redex/stepper]
                               [traces redex/traces]
                               [current-traced-metafunctions redex/current-traced-metafunctions]))]

@(require scribble/eval)

Last Updated @(date->string (seconds->date (current-seconds))).

@title{A Gradual Dependently Typed Language}

This is a thin set of macros provided on top of the redex-model for the Gradual Dependently-typed language.

Right now, this is only intended for playing around with examples from the paper. Notably, it's very slow.
Compile-time and runtime error messages are okay, but might be informative in some cases.

@defmodule[GDTL/main]

@section{Getting Started}

Import as @racketblock{#lang s-exp GDTL} 

@section{Overview of the language}

@(racketgrammar*
   #:literals (: ? ::)
   [term (lambda (id) term) 
         (term term)
         (-> (id : term) term)
         (:: term term )
         (Set natural)
         Nat
         Zero
         (Succ term)
         (NatElim natural term term term id term)
         (Vec type term)
         (Nil term)
         (Cons type term term term)
         (VecElim natural term term term term id id id id term)
         (Eq type term term)
         (Refl type term)
         (EqElim natural term type term id term term term)
         ?]
   [type term])

@section{Specific forms}

@defform[(Set i)
         ]{
  The type of types at level @racket[i].
}

@defidform[?
         ]{
  The dynamic (gradual) type.
}

@defform[(lambda (x ...) body)
         ]{
  Defines a function taking arguments @racket[x ...] with body @racket[body]
}


@defform*[#:literals (->)
          ((-> (id : dom) cod)
          (-> dom cod)
          (-> argtype ... cod))
         ]{
@(racketgrammar*
   #:literals (:)
   [argtype term (id : term)])

First form: the dependent function type taking an argument named @racket[id] of type @racket[dom] to a value of type @racket[cod]. 

  Second form: sugar for the non-dependent function type.


   Third form: Sugar for @racket[(-> argtype1 (-> argtype2 ... -(> argtype_n cod) ... ) )].
}



@defform[(:: type term)
         ]{
  Behaves as @racket[term], but informs the compiler that it has type @racket[type].
}

@defidform[Nat
         ]{
  The type of natural numbers. Digits are automatically converted into chains of @racket[(Succ (Succ ... (Succ Zero)))]
}

@defidform[Zero
         ]{
  The constant 0.
}

@defform[ (Succ term)
         ]{
  One greater than the given term.
}

@defform*[
 ((NatElim discriminee motive zero-case id-less id-recursive succ-case )
          (NatElim level discriminee motive zero-case id-less id-recursive succ-case ))
         ]{
  Case analysis/induction for natural numbers. @racket[discriminee] should be a @racket[Nat].
 The @racket[level] argument is optional, and defaults to 1.
  The @racket[motive] should have type @racket[(-> Nat (Set level))]. If the discriminee is @racket[0],
  then @racket[zero-case] is returned. If the discriminee is @racket[(Succ n)], then @racket[n]
  is bound to @racket[id-less : Nat], and the result of @racket[(NatElim n motive zero-case id-less id-recursive succ-case )]
  is bound to @racket[id-recursive : (motive id-less)] in @racket[succ-case : (motive (Succ id-less))], which is then returned.

  Produces a result of type @racket[(motive discriminee)].
  
}

@defform[(Vec element-type length)
         ]{
  The type of vectors containing @racket[length] elements with type @racket[element-type], where 
                                 @racket[element-type : (Set 1)] and @racket[length : Nat]. 
}

@defform[(Nil element-type)
         ]{
  The 0-length vector for @racket[element-type].
}

@defform[ (Cons element-type length head tail)
         ]{
  Extends @racket[tail : (Vec element-type length)] with element @racket[head : element-type]. 
}

@defform*[
 ((VecElim discriminee element-type length motive nil-case id-length id-head id-tail id-rec cons-case )
          (VecElim level discriminee element-type length motive nil-case id-length id-head id-tail id-recursive cons-case ))
         ]{
  Case analysis/induction for vectors. @racket[discriminee] should be a @racket[(Vec element-type length)].
 The @racket[level] argument is optional, and defaults to 1.
  The @racket[motive] should have type @racket[(-> (n : Nat) (Vec element-type n ) (Set level))]. If the discriminee is @racket[(Nil element-type)],
  then @racket[nil-case] is returned. If the discriminee is @racket[(Cons element-type n head tail)], then @racket[n]
  is bound to @racket[id-length : Nat], @racket[head] to @racket[id-head : element-type], @racket[tail] to @racket[id-tail : (Vec element-type id-length )],
   and the result of @racket[(VecElim tail element-type length motive nil-case id-length id-head id-tail id-rec cons-case )]
  is bound to @racket[id-recursive : (motive id-length id-tail)] in @racket[succ-case : (motive (Succ id-length) (Cons element-type id-length id-head id-tail))], which is then returned.

  Produces a result of type @racket[(motive length discriminee)].
  
}

@defform[(Eq A term1 term2)
         ]{
  The type of proofs that @racket[term1] and @racket[term2] are propositionally equal,
                          where @racket[term1 : A] and @racket[term2 : A].
}

@defform[(Refl A term)
         ]{
  The reflexive proof that @racket[term] is equal to itself.
                           Has type @racket[Eq A term term].
}

@defform*[
 ((EqElim proof A motive id refl-case term1 term2 )
          (EqElim level proof A motive id refl-case term1 term2 ))
         ]{
  The J axiom for equality. @racket[proof] should have type @racket[Eq A term1 term2].
 The @racket[level] argument is optional, and defaults to 1.
 @racket[A] should be a type. 
  The @racket[motive] should have type @racket[(-> (x : A) (y : A) (Eq A x y) (Set i))].
  The identifier  @racket[z] is bound in @racket[refl-case], which should have type @racket[(motive z z (Refl A z z))]. 
  The terms @racket[term1] and @racket[term2] that are equated by @racket[proof] must be given explicitly as well.

  Produces a result of type @racket[(motive term1 term2 pf)].
  
}

@section{Top level commands}

Any bare terms at the top-level are elaborated and evaluated.
However, there are other directives we can use at the top level.

@defform*[((define id body)
           (define (id : type) (id x ... = body)))
         ]{
  First form: defines @racket[id] to be @racket[body] at the top level.

 Second form: defines @racket[id] to be a function of type @racket[type] taking arguments @racket[x ...] to result @racket[body].
  This is just sugar for @racket[(define id (:: (lambda (x...) body) type))].

}

@defform[(trace-on)]{
Turn on printing of all steps tried during typechecking and evaluation.
Equivalent to @racket[(redex/current-traced-metafunctions 'all)].
}

@defform[(trace-off)]{
Turn off printing of all steps tried during typechecking and evaluation.
Equivalent to @racket[(redex/current-traced-metafunctions '())]
}

@defform[(stepper term)]{
Run a  @racket[redex/stepper] to show the evaluation of @racket{term}.
}

@defform[(traces term)]{
Run @racket[redex/traces] on the evaluation of @racket{term}.
}
 
@defform[(normalize term)]{
Display the compile-time normalization of @racket{term}.
}

@section{Examples}

Wtih s-expressions:

@codeblock{
#lang s-exp GDTL
(define
  (loop : (->
           (A : (Set 10))
           (B : (Set 10))
           (-> (-> A B) A B )
           ?
           A
           B))
   (loop A B f x = (f (lambda (y) (x x y))))
  )

(define
   (Z : (->
         (A : (Set 10))
         (B : (Set 10))
         (-> (-> A B) (-> A B ))
         (-> A B)))
   (Z A B f = ((loop A B f) (loop A B f)))
  )
 }


We can use gradual inductive types. For example,
here we use the unknown term @racket[?] as a vector length,
to apply the vector @racket[head] function to a vector
with unknown length. At runtime, dynamic checks ensure that
we throw an exception if the vector is empty.

@codeblock{
#lang s-exp GDTL


;;We can eliminate over natural numbers to define iteration or perform induction
;;e.g. addition can be defined as follows:
(define
  (plus : (-> Nat Nat Nat))
  ( plus m n = (NatElim m (lambda (x) Nat) n m-1 rec (Succ rec))
         ))

;;We can even do large elimination, to choose a type from a number
;;This lets us define the safe head function
(define
  (ifzero1 : { -> Nat (Set 1) (Set 1) (Set 1) } )
  (ifzero1 n S1 S2 = (NatElim 2 n (lambda (x) (Set 1)) S1 x1 x2 S2)
           ))
;;We eliminate a vector, producing 0:Nat if it is empty,
;; and producing its first element otherwise
;; This is type-safe because the motive of our elimination is dependent on the length of the vector
(define
  (head : {-> (A : (Set 1)) (n : Nat) (Vec A (Succ n))  A })
  (head A n v = (VecElim v A (Succ n) (lambda (n2 v) (ifzero1 n2 Nat A)) 0 x1 x_head x3 x4 x_head )))

;; An empty vector with  length
(define safeNil {:: (Nil Nat)  (Vec Nat 0)})

;; A non-empty vector with known length
(define safeCons {:: (Cons Nat 0 2 (Nil Nat))  (Vec Nat 1)})

;; A vector with unknown length
(define unsafeNil {:: (Nil Nat)  (Vec Nat ?)})

;; Another vector with unknown length
(define unsafeCons {:: (Cons Nat ? 2 (Nil Nat))  (Vec Nat ?)})

;;Type-checks and evaluates to 2
(head Nat 0 safeCons)

;;Does not typecheck
(head Nat ? safeNil)

;;Type-checks and evaluates to 2
(head Nat ? unsafeCons)


;;Type-checks but throws runtime exception
(head Nat ? unsafeNil)


;; We can write normal static equalities
(define 1+1=2 {:: (Refl Nat 2)
                  (Eq Nat 2 (plus 1 1))})



;;And a generic substitution principle
(define
  (subst : (-> (A : (Set 1)) (P : (-> A  (Set 1))) (x : A) (y : A) (Eq A x y) (P x) (P y) ))
  (subst A P x y pf = (EqElim
                       pf
                       A
                       (lambda (x1 y1 pf1) (-> (P x1) (P y1)))
                       z (lambda (Pz) Pz)
                       x
                       y
                       )))

;; And statically they fail if the two terms aren't equal
;;i.e. this does not compile
;(define 1+1=1bad {:: (Refl Nat 1)
;                  (Eq Nat 1 (plus 1 1))})


;;But we can use gradual types to type untrue equalities
(define 0=1 {:: (Refl Nat ?)
                  (Eq Nat 0 1)})

;;And define functions that are statically impossible
(define (magic : (-> (A : (Set 1)) (Vec A 0) (Vec A 1)))
  (magic A v = (subst
  Nat
  (lambda (n) (Vec A n))
  0
  1
  0=1
  v
 )))


;;But since our language is safe, calling these impossible functions
;; will result in a runtime error runtime error
(magic Nat (Nil Nat))

           }

Or we can build up booleans, natural numbers, and vectors using Church encodings.
This file also gives a flavour of what the language looks like with Sweet Expressions
instead of S-expressions.


@codeblock{
#lang sweet-exp GDTL

define
  bool : Set(5)
  bool = {(A : Set(4)) A A -> A}

define
  true : bool
  true A x y = x

define
  false : bool
  false A x y = y

define
  if : {(A : Set(4)) bool A A -> A}
  if A b x y =
    b A x y

define
  nat : Set(6)
  nat = {(A : Set(5)) {A -> A} A -> A}

define
  zero : nat
  zero A f z = z

define
  succ : {nat -> nat}
  succ n A f z =
    f (n A f z)

define
  zero? : {nat -> bool}
  zero? n =
    n bool (lambda (x) false) true 

define
  vec : {Set(6) nat -> Set(6)}
  vec A n = { (M : {nat -> Set(4)}) {(m : nat) A M(m) -> M(succ(m))} M(zero) -> M(n) }


define
  head : { (A : Set(3)) (n : nat) (vec(A succ(n))) -> A}
  head A n v = (v 
                (lambda (m)
                  (if Set(3) (zero? m) Set(2) A)
                  )
                (lambda (m elem prev) elem)
                Set(1)
                )      
        


define
  nil : {(A : Set(6)) -> vec(A zero)}
  nil A M f mz = mz


define
  cons : {(A : Set(6)) (n : nat) (hd : A) (tl : vec(A n)) -> vec(A succ(n) )}
  cons A n hd tl M f mz = (f n hd (tl M f mz))


define
  unsafeNil : vec(Set(2) ?)
  unsafeNil = { (nil Set(2)) :: vec(Set(2) ?)}


 (head Set(2) zero unsafeNil)
}

The final line typechecks, but gives a runtime type-error.



