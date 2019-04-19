#lang scribble/manual

@require[(for-label GDTL/main)]
@require[(for-label (rename-in redex
                               [stepper redex/stepper]
                               [traces redex/traces]
                               [current-traced-metafunctions redex/current-traced-metafunctions]))]

@(require scribble/eval)

@title{Gradual types with Lambda and Pi}

This is a thin set of macros provided on top of the redex-model for the Gradual Dependently-typed language.

Right now, this is only intended for playing around with examples from the paper. Notably, it's very slow.
Compile-time and runtime error messages are okay, but might be ininformative in some cases.

@defmodule[GDTL/main]

@section{Getting Started}

Import as @racketblock{#lang s-exp GDTL} or @racketblock{#lang sweet-exp GDTL}

@section{Overview of the language}

@(racketgrammar*
   #:literals (: ?)
   [term (lambda (id) term) 
         (term term)
         (-> (id : term) term)
         (:: term term )
         (Set natural)
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
   If using sweet-expressions, this can be written as
   @racket[{argtype ... -> cod}]
}



@defform[(:: type term)
         ]{
  Behaves as @racket[term], but informs the compiler that it has type @racket[type].
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

Using sweet-expressions:

@codeblock{
#lang sweet-exp GDTL

define
   loop : {(A : Set(10)) (B : Set(10)) {{A -> B} -> {A -> B} } ? -> {A -> B} }
   loop A B f x = (f (lambda (y) (x x y)))

define
   Z : {(A : Set(10)) (B : Set(10)) {{A -> B} -> {A -> B} } -> {A -> B} }
   Z A B f = ((loop A B f) (loop A B f))
 }

We can build up booleans, natural numbers, and vectors using Church encodings.


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



