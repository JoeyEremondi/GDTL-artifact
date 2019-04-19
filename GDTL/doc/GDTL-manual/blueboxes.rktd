550
((3) 0 () 1 ((q lib "GDTL/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q trace-off)) q (242 . 2)) ((c form c (c (? . 0) q normalize)) q (306 . 2)) ((c form c (c (? . 0) q ?)) q (16 . 2)) ((c form c (c (? . 0) q trace-on)) q (224 . 2)) ((c form c (c (? . 0) q lambda)) q (25 . 2)) ((c form c (c (? . 0) q stepper)) q (261 . 2)) ((c form c (c (? . 0) q define)) q (151 . 3)) ((c form c (c (? . 0) q Set)) q (0 . 2)) ((c form c (c (? . 0) q ::)) q (127 . 2)) ((c form c (c (? . 0) q ->)) q (57 . 4)) ((c form c (c (? . 0) q traces)) q (284 . 2))))
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
