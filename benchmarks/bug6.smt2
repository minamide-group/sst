(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)


(assert (= x1 (str.replace x0 "a" "b")))

(assert (= x2 (str.++ "a" x1 "a")))

(assert (str.in.re x2 (re.++ (str.to.re "a")
							 (re.* (str.to.re "b"))
							 (str.to.re "a")
)))

(check-sat)
(get-model)