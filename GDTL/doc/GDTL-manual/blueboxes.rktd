905
((3) 0 () 1 ((q lib "GDTL/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q NatElim)) q (194 . 3)) ((c form c (c (? . 0) q Succ)) q (174 . 2)) ((c form c (c (? . 0) q trace-off)) q (803 . 2)) ((c form c (c (? . 0) q Vec)) q (360 . 2)) ((c form c (c (? . 0) q Set)) q (0 . 2)) ((c form c (c (? . 0) q ::)) q (127 . 2)) ((c form c (c (? . 0) q stepper)) q (822 . 2)) ((c form c (c (? . 0) q Nat)) q (151 . 2)) ((c form c (c (? . 0) q Nil)) q (395 . 2)) ((c form c (c (? . 0) q Zero)) q (162 . 2)) ((c form c (c (? . 0) q define)) q (712 . 3)) ((c form c (c (? . 0) q trace-on)) q (785 . 2)) ((c form c (c (? . 0) q lambda)) q (25 . 2)) ((c form c (c (? . 0) q normalize)) q (867 . 2)) ((c form c (c (? . 0) q Cons)) q (422 . 2)) ((c form c (c (? . 0) q traces)) q (845 . 2)) ((c form c (c (? . 0) q ?)) q (16 . 2)) ((c form c (c (? . 0) q ->)) q (57 . 4)) ((c form c (c (? . 0) q VecElim)) q (470 . 3))))
syntax
(Set i)
syntax
?
syntax
(lambda (x ...) body)
syntax
(-> (id : dom) cod)
(-> dom cod)
(-> argtype ... cod)
syntax
(:: type term)
syntax
Nat
syntax
Zero
syntax
(Succ term)
syntax
(NatElim discriminee motive zero-case id-less id-recursive succ-case)
(NatElim level discriminee motive zero-case id-less id-recursive succ-case)
syntax
(Vec element-type length)
syntax
(Nil element-type)
syntax
(Cons element-type length head tail)
syntax
(VecElim discriminee element-type length motive nil-case id-length id-head id-tail id-rec cons-case)
(VecElim level discriminee element-type length motive nil-case id-length id-head id-tail id-recursive cons-case)
syntax
(define id body)
(define (id : type) (id x ... = body))
syntax
(trace-on)
syntax
(trace-off)
syntax
(stepper term)
syntax
(traces term)
syntax
(normalize term)
