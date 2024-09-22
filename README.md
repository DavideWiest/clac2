
## The Clac Programming Language
- functional
- simple

### Features
- **Overloaded forwarding**: `add 1 mul 2 3` evaluates to seven
- **Type auto-expension**: the type signature "int" suffices for plus. The type "int" is duplicated to match the number of arguments. This only happens if the signature is 1 long (soon)
- Custom operators (infix functions) (soon)

### Ideas
- type-multiplication: "int*3" for "int int int" -> most likely not
- shell
- package manager
- clacObj folder (saving parsed states)
    - memoization of evaluations -> useful enough?
