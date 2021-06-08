.section .text
.globl main
main:
		li a0, 32
		call malloc
		sd a0, -8(sp)
		li t3, 1
		sd t3, 0(sp)
		jal newscope
		ld t1, 0(sp)
		ld t0, -8(sp)
		sd t1, 8(t0)
		li t3, 2
		sd t3, 0(sp)
		jal newscope
		ld t1, 0(sp)
		ld t0, -8(sp)
		sd t1, 8(t0)
		li t3, 3
		sd t3, 0(sp)
		jal newscope
		ld t1, 0(sp)
		ld t0, -8(sp)
		sd t1, 16(t0)
		li t3, 4
		sd t3, 0(sp)
		nop
		ld t4, -8(sp)
		ld t5, 0(t4)
		sd t5, -8(sp)
		nop
		ld t4, -8(sp)
		ld t5, 0(t4)
		sd t5, -8(sp)
		nop
		ld t4, -8(sp)
		ld t5, 0(t4)
		sd t5, -8(sp)
		la a0, msg
		ld a1, 0(sp)
		call printf
		li a0, 0
		jal exit
		newscope:
		li a0, 32
		mv s1, ra
		call malloc
		mv ra, s1
		mv t1, a0
		ld t0, -8(sp)
		sd t1, -8(sp)
		sd t0, 0(t1)
		li t3, 2
		loop6:
		addi t1, t1, 8
		addi t1, t1, 8
		ld t4, 0(t0)
		sd t4, 0(t1)
		addi t3, t3, -1
		bgez t3, loop6
		jr ra
.section .rodata
msg:
		.string "Result: %d\n"
 