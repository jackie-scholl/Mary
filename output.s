.section .text
.globl main
main:
		addi sp, sp, -48
		li a0, 24
		call malloc
		sd a0, -8(sp)
		li t3, 3
		sd t3, 0(sp)
		jal newscope
		ld t4, 0(sp)
		li t5, 0
		addi t5, t5, 1
		slli t5, t5, 3
		ld t6, -8(sp)
		add t6, t5, t6
		sd t4, 0(t6)
		li t3, 4
		sd t3, 0(sp)
		ld t4, -8(sp)
		ld t5, 0(t4)
		sd t5, -8(sp)
		la a0, msg
		ld a1, 0(sp)
		call printf
		li a0, 0
		jal exit
		newscope:
		li a0, 24
		mv s1, ra
		call malloc
		mv ra, s1
		mv t5, a0
		ld t4, -8(sp)
		ld t6, 0(t4)
		sd t5, -8(sp)
		sd t6, 0(t5)
		li a0, 1
		loop6:
		addi t5, t5, 8
		addi t4, t4, 8
		ld t6, 0(t4)
		sd t6, 0(t5)
		addi a0, a0, -1
		bgez a0, loop6
		jr ra
.section .rodata
msg:
		.string "Result: %d\n"
 