.section .text
.globl main
main:
		li a0, 1048576
		call malloc
		mv s0, a0
		addi s0, s0, 8
		li a0, 40
		call malloc
		sd a0, -8(s0)
		li t3, 1
		sd t3, 0(s0)
		jal newscope
		ld t1, 0(s0)
		ld t0, -8(s0)
		sd t1, 8(t0)
		li t3, 2
		sd t3, 0(s0)
		jal newscope
		ld t1, 0(s0)
		ld t0, -8(s0)
		sd t1, 16(t0)
		li t3, 4
		sd t3, 0(s0)
		jal newscope
		ld t1, 0(s0)
		ld t0, -8(s0)
		sd t1, 24(t0)
		li t3, 8
		sd t3, 0(s0)
		jal newscope
		ld t1, 0(s0)
		ld t0, -8(s0)
		sd t1, 8(t0)
		ld t0, -8(s0)
		ld t2, 16(t0)
		sd t2, 0(s0)
		ld t0, -8(s0)
		ld t2, 8(t0)
		sd t2, 8(s0)
		ld t4, 0(s0)
		ld t5, 8(s0)
		ADD t3, t4, t5
		sd t3, 0(s0)
		nop
		ld t4, -8(s0)
		ld t5, 0(t4)
		sd t5, -8(s0)
		nop
		ld t4, -8(s0)
		ld t5, 0(t4)
		sd t5, -8(s0)
		nop
		ld t4, -8(s0)
		ld t5, 0(t4)
		sd t5, -8(s0)
		nop
		ld t4, -8(s0)
		ld t5, 0(t4)
		sd t5, -8(s0)
		la a0, msg
		ld a1, 0(s0)
		call printf
		li a0, 0
		jal exit
		newscope:
		li a0, 40
		mv s1, ra
		call malloc
		mv ra, s1
		mv t1, a0
		ld t0, -8(s0)
		sd t1, -8(s0)
		sd t0, 0(t1)
		li t3, 3
		loop6:
		addi t0, t0, 8
		addi t1, t1, 8
		ld t4, 0(t0)
		sd t4, 0(t1)
		addi t3, t3, -1
		bgez t3, loop6
		jr ra
.section .rodata
msg:
		.string "Result: %d\n"
 