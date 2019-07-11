(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const x4 String)
(declare-const x5 String)
(declare-const i Int)


(assert (= x1 "cd" ))
(assert (= x2 (str.replaceall x1 "c" "f")))
(assert (str.in.re x0 (re.*  re.allchar)))
(assert (= 8 i))
(assert (= i (str.len x0)))


(check-sat)
(get-model)
