#lang sweet-exp glp

(define Set2 Set(2))

 (stepper (
 {(lambda x x)
 :: {(x : Set2) -> Set2 }
 } Set(1)))
