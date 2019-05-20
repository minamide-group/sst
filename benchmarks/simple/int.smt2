(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

;(assert (= (str.substr x0 3) x1))
;(assert (< -1 (str.len x1)))
(assert (= i 1))

(check-sat)
(get-model)