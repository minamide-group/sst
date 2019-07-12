# String Constraint Solver

A string constraint solver supporting concatenation and transduers.

## Getting Started

### Prerequisites

Z3 SMT solver : https://github.com/Z3Prover/z3.

## Language

### Declare String Variables
```
(declare-const x String)
```
or
```
(declare-fun x() String)
```

### String Concatenation
```
str.++ s1 s2 ...
```
**s1** and **s2** are either string variables or constant strings (e.g. "abc").


### String Variable Character At Index
```
str.at x idx
```
**x** is a string variable. 
**idx** is an integer.


### String Variable Substring
```
str.substring x begin count
```
```
str.substring x begin
```
**x** is a string variable. 
**begin** and **count** are integers.

### String Variable Reverse
```
str.reverse x
```
**x** is a string variable.

### String Variable Insertion
```
str.insert x idx str
```
**x** is a string variable. 
**idx** is an integer. 
**str** is a constant string.

### String Variable Replacement
```
str.replace x str1 str2
```
```
str.replaceall x str1 str2
```
**x** is a string variable. 
**str1** and **str2** are constant strings.


### String Variable Length
```
str.len x
```
**x** is a string variable. 

### String Variable Regular Membership
```
str.in.re x regex
```
**x** is a string variable. 

**regex** is a regular expresstion defined by following operations :
```
str.to.re str
```
**str** is a constant string.

```
re.++ regex1 regex2
```
Now **re.++** takes one or more arguments.

```
re.union regex1 regex2
```
Now **re.union** takes one or more arguments.

```
re.* regex
```

```
re.+ regex
```

```
re.range s t
```

```
re.allchar
```
**re.allchar** represents any symbol in the input constraint.


## Running the test

Suppose we have a file substr.smt2 with following content:
```
(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

(assert (= (str.substr x0 3) x1))
(assert (str.in.re x0 (str.to.re "aba")))
(assert (= 0 (str.len x1)))

(check-sat)
(get-model)
```

Execute:
```
checker substr.smt2
```
or
```
java -jar checker.jar substr.smt2
```

then the result is:
```
sat
(model
  (define-fun x0 () String
    "aba")
  (define-fun x1 () String
    "")
)

```

### Specify ASCII:
```
checker substr.smt2 -ascii i
```
**i** is an integer ranging from 0 to 256. The solver will simply
add the characters from 0 to **i-1** to the alphabet.

### Print Details in SST Composition:
```
checker substr.smt2 -p
```
```
SST number: 1
SST List:
Q: 5,  X: 2, Delta: 4, Eta: 4

sat
(model
  (define-fun x0 () String
    "aba")
  (define-fun x1 () String
    "")
)
```

### Print Parikh:
```
checker substr.smt2 -parikh
```
