#lang sweet-exp glp

define
  bool : Set(2)
  bool = {(A : Set(1)) A A -> A}

define
  true : bool
  true A x y = x

define
  false : bool
  false A x y = y

define
  if : {(A : Set(1)) bool A A -> A}
  if A b x y =
    b A x y

define
  nat : Set(3)
  nat = {(A : Set(2)) {A -> A} A -> A}
;
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
  vec : {Set(4) nat -> Set(4)}
  vec A n = { (M : {nat -> Set(3)}) {(m : nat) A M(m) -> M(succ(m))} M(zero) -> M(n) }



define
  nil : {(A : Set(4)) -> vec(A zero)}
  nil A M f mz = mz

define
  foo : ?
  foo = zero



define
  cons : {(A : Set(4)) (n : nat) (hd : A) (tl : vec(A n)) -> vec(A succ(n) )}
  cons A n hd tl M f mz = (f n hd (tl M f mz))
