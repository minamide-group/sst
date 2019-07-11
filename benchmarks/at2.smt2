(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

(assert (and 
			(= x1  (str.at x0 3))
			(not (= 1 (str.len x1)))	
		)
)


(check-sat)