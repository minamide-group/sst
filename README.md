# String Constraint Solver

A string constraint solver supporting concatenation and transduers.

## Getting Started

### Prerequisites

Z3 SMT solver : https://github.com/Z3Prover/z3.

## Language

### Declare variables
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
s1 and s2 are either string variables or constant strings (e.g. "abc").


### String Variable Character At Index
```
str.at x idx
```
x is a string variable, idx is a integer value.


### String Variable Substring
```
str.substring x begin count
```

```
str.substring x begin
```


### String Variable Reverse
```
str.reverse x
```


### String Variable Insertion
```
str.insert x idx str
```


### String Variable Replacement
```
str.replace x str1 str2
```

```
str.replaceall x str1 str2
```

### String Variable Length
```
str.len x
```

### String Variable Regular Membership
```
str.in.re x regex
```

```
str.to.re str
```

```
re.++ regex1 regex2
```

```
re.union regex1 regex2
```

```
re.* regex
```


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