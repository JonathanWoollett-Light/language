.global _start
_start:
mov x8, #64
mov x0, #1
mov x1, =a
mov x2, #14
svc #0
mov x8, #93
mov x0, #0
svc #0
.data
a: .byte 72,101,108,108,111,44,32,87,111,114,108,100,33,10
b: .byte 1
g: .byte 0
