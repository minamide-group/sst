;fixed

(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const i Int)


(assert (= x0 "a"))
(assert (= x1 "ab"))


(check-sat)
(get-model)