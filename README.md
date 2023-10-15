# language

[![GitHub deployments](https://img.shields.io/github/deployments/jonathanwoollett-light/language/github-pages?label=website)](https://jonathanwoollett-light.github.io/language/)

- **Simple**
  Python-like syntax with fewer keywords than C.
- **Explicit**
  No hidden memory allocations, not even on the stack. No stack overflow.
- **Thorough**
  Type inference incorporating formal verification. No generics or templates.
- **Interpretable**
  Compile-time evaluation of syscalls. No build scripts. No macros.

### Hello, World!

```
x := "Hello, World!\n"
_ := write 1 x
exit 0
```
