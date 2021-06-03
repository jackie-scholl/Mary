.section .text
.globl main
main:
		addi sp, sp, -48
		li t3, 1
		sd t3, 0(sp)
		li a0, 128
		call malloc
		sd a0, 0(sp)
		la a0, msg
		ld a1, 0(sp)
		call printf
		li a0, 0
		jal exit
.section .rodata
msg:
		.string "Result: %d\n"
 