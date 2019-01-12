#lang sweet-exp glp


define
  nat : Set(2)
  nat = {(A : Set(1)) {A -> A} A -> A}
;
define
  zero : nat
  zero A f z = z

define
  succ : {nat -> nat}
  succ n A f z =
    f (n A f z)

nat
zero


succ

