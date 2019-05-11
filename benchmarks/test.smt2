(declare-const x0 String)
(declare-const x1 String)
(declare-const y String)
(declare-const i Int)


(assert (= x1  (str.insert x0 1 "2")))
(assert (str.in.re x0 (str.to.re "aabc")))
(assert (str.in.re x1 (str.to.re "a2abc")))
(assert (= i 10))
(assert (= i (str.len y)) )

(check-sat)
(get-model)