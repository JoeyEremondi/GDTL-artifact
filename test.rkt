#lang sweet-exp glp



;(trace-on)

;(head Set(2) zero (cons Set(2) zero Set(1) (nil Set(2))))

define
  unsafeNil : vec(Set(2) ?)
  unsafeNil = { (nil Set(2)) :: vec(Set(2) ?)}

;(trace-on)

(traces (head Set(2) zero unsafeNil))



