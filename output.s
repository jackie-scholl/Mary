.section .text
.globl main
main:
		addi sp, sp, -48
		li t3, 1
		sd t3, 0(sp)
		la a0, msg
		ld a1, 0(sp)
		jal ra, printf
		li a0, 0
		jal exit
.section .rodata
msg:
		.string "Result: %d\n"
 