# regexp-to-nfa-compiler
Programming Languages Project - UniMiB

Compiler from Regular Expression to NFA (Nondeterministic Finite Automaton), developed both in Prolog and Common Lisp\
Below is an explaination of how to represent _sequence_, _or_, _Kleene star_ and _plus_ (one or more repetitions)

**Prolog**
- Sequence: seq(<re1>,<re2>,..., <rek>)
- Or:       or(<re1>, <re2>, ..., <rek>)
- Star:     star(<re>)
- Plus:     plus(<re>)
