(declare-const x String)
(declare-const y String)
(declare-const z String)
(declare-const w String)

(assert (str.in.re x (str.to.re "a")))
(assert (str.in.re y (str.to.re "b")))

(assert (= 1 (str.len x)))

(assert (= 1 (str.len z)))


(check-sat)
(get-model)