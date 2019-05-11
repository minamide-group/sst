(declare-const x String)
(declare-const y String)
(declare-const z String)
(declare-const w String)


(assert (str.in.re x (str.to.re "a")))

(check-sat)
(get-model)