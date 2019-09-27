(* (* a (+ b c)) (- d e)) -- prefix
((a (b c +) *) (d e -) *) -- postfix
-----------------------------
(- (* 2 (sqrt (- (/ (/ b 2) (/ b 2)) (* (* 4 a) c)))) a) -- prefix
((2 ((((b 2 /) (b 2 /) /) ((4 a *) c *) -) sqrt) *) a -) -- postfix