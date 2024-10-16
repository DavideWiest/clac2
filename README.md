
## The Clac Programming Language
- functional
- simple
- **still under development**

### Features
- **Type auto-expension**: the type signature "int" suffices for plus. The type "int" is duplicated to match the number of arguments. This only happens if the signature is 1 long (soon)
- **Custom operators** (prefix, postfix, infix) (soon)
- **Type multiplication**: "int*3" for "int int int"
- **Argument propagation**: "mul 3 mul 3 2" equals 18

### Ideas
- shell
- package manager
- clacObj folder (saving parsed, validated, prepared files with hash)
    - memoization of evaluations -> useful enough?
