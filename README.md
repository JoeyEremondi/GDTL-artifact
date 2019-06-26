# A Gradual Dependently Typed Language

Code accompanying "Approximate Normalization for Gradual Dependently-Typed Languages".

To install:


* [Download](https://github.com/JoeyEremondi/GDTL-artifact/archive/master.zip) and unzip, or run `git clone https://github.com/JoeyEremondi/GDTL-artifact.git`. 
* Open [Dr. Racket](https://racket-lang.org/) and select Install Package.. in DrRacket’s File menu.
* Click Browse and then Directory (in the dialog that appears), and then select `path/to/download/GDTL`

The documentation and examples are in `path/to/download/GDTL/doc/GDTL-manual/index.html`.
Or you can [view them online](http://eremondi.com/GDTL-artifact/GDTL/doc/GDTL-manual/index.html).

Relevant files:

* `examples_inductive.rkt`: contains the examples of `Vec Nat ?` from the paper, showing examples of imprecisely-typed vectors. Ther eis one that fails to typecheck, one that typechecks but throws a runtime error, and an example that succeeds at runtime.
* `examples.rkt`: similar examples, but using Church-encodings. This file is pretty slow to compile and run.
* `GDTL/main.rkt`: The Racket macros implementing the main syntax of the language
* `GDTL/lang_simple.rkt`: The Redex model containing the bulk of the implementation of the language. This closely matches the definitions from the paper, though some parts have been hand-implemented for efficiency.
* `lang_simple.ott`: The Ott source used to generate the Redex model. Requires a [custom fork of Ott](https://github.com/JoeyEremondi/ott/tree/redexFixes).
