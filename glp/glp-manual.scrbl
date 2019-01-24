#lang scribble/manual

@require[(for-label glp/main)]
@require[(for-label (rename-in redex
                               [stepper redex/stepper]
                               [traces redex/traces]
                               [current-traced-metafunctions redex/current-traced-metafunctions]))]

@title{Gradual types with Lambda and Pi}

This is a thin set of macros provided on top of the redex-model for the Gradual Dependently-typed language.

Right now, this is only intended for playing around with examples from the paper. Notably, it's *very* slow
and the type-error messages are incomprehensible.

@defmodule[glp/main]

@section{Getting Started}

Import as @racketblock{#lang s-exp glp} or @racketblock{#lang sweet-exp glp}

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



