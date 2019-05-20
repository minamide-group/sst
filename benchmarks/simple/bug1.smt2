;fixed

(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const i Int)


(assert (= x1 "a"))
(assert (= x2 x0))
(assert (str.in.re x2 (str.to.re "baa")))

(check-sat)
(get-model)