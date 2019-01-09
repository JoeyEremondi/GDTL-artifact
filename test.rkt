#lang sweet-exp glp


define
  nat : Set(2)
  nat = {(A : Set(1)) {A -> A} A -> A}

define
  zero : nat
  zero A f z = z

(zero)