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

#### Source

```
x := "Hello, World!\n"
_ := write 1 x
exit 0
```

#### Assembly

```
.global _start
_start:
mov x8, #64
mov x0, #1
ldr x1, =x
mov x2, #14
svc #0
mov x8, #93
mov x0, #0
svc #0
.data
x: .byte 72,101,108,108,111,44,32,87,111,114,108,100,33,10
```
