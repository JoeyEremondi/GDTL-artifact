#lang sweet-exp glp

;(trace-on)

define
   loop : {(A : Set(10)) (B : Set(10)) {{A -> B} -> {A -> B} } ? -> {A -> B} }
   loop A B f x = (f (lambda (y) (x x y)))

;(trace-on)

define
   Z : {(A : Set(10)) (B : Set(10)) {{A -> B} -> {A -> B} } -> {A -> B} }
   Z A B f = ((loop A B f) (loop A B f))

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

;define 
;  iff : {(A : bool -> Set) b:bool A(true) A(false) -> A(b)}
;  iff A b x y = ...
     

define
  nat : Set(6)
  nat = {(A : Set(5)) {A -> A} A -> A}
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
  vec : {Set(6) nat -> Set(6)}
  vec A n = { (M : {nat -> Set(4)}) {(m : nat) A M(m) -> M(succ(m))} M(zero) -> M(n) }

;trace-on()

define
  head : { (A : Set(3)) (n : nat) (vec(A succ(n))) -> A}
  head A n v = (v 
                (lambda (m)
                  (if Set(3) (zero? m) Set(2) A)
                  )
                (lambda (m elem prev) elem)
                Set(1)
                )      
        
trace-off()

define
  nil : {(A : Set(6)) -> vec(A zero)}
  nil A M f mz = mz


define
  cons : {(A : Set(6)) (n : nat) (hd : A) (tl : vec(A n)) -> vec(A succ(n) )}
  cons A n hd tl M f mz = (f n hd (tl M f mz))


define
  unsafeNil : vec(Set(2) ?)
  unsafeNil = { (nil Set(2)) :: vec(Set(2) ?)}

;trace-on()
(head Set(2) zero unsafeNil)

define
  safeCons : vec(Set(2) ?)
  safeCons = { (cons(Set(2) zero Set(1) (nil Set(2)) )) :: vec(Set(2) ?)}

;trace-on()

; ET: this takes for ever
(head Set(2) zero safeCons) 
