(declare-const x0 String)
(declare-const x1 String)
(declare-const x4 String)
(declare-const x5 String)

(assert (= x1 (str.replaceall x0 "<script>" "x")))
(assert (= x5 (str.replaceall x1 "<script>" "x")))
(assert (= x4 (str.++ x5 x5)))
(assert (str.in.re x4 (re.++ 
(re.* (re.range "a" "z")) 
(re.++ (str.to.re "<script>") (re.* (re.range "a" "z")) )                
          )
)
)

(check-sat)
(get-model)