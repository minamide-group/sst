(declare-const x String)
(declare-const y String)
(declare-const z String)
(declare-const w String)


(assert (str.in.re x (re.+ (re.range "a" "z"))))
(assert (str.in.re x (str.to.re "ab") ))

(check-sat)
(get-model)