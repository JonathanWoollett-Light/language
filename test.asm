.global _start
_start:
ldr x0, =e
mov w1, #1
strb w1, [x0]
mov x8, #93
ldr x0, =e
ldrb w0, [x0]
svc #0
.bss
e: .skip 1
