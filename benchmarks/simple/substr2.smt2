(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

(assert (= (str.substr x0 -1) x1))
(assert (str.in.re x0 (str.to.re "aba")))
(assert (= 0 (str.len x1)))


(check-sat)