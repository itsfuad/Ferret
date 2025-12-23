.data
.align 8
str1:
	.ascii "170141183460469231731687303715884105727"
	.byte 0
/* end data */

.data
.align 8
str2:
	.ascii "1"
	.byte 0
/* end data */

.data
.align 8
str3:
	.ascii "0"
	.byte 0
/* end data */

.data
.align 8
str4:
	.ascii "i128 ok"
	.byte 0
/* end data */

.data
.align 8
str5:
	.ascii "79228162514264337593543950336"
	.byte 0
/* end data */

.data
.align 8
str6:
	.ascii "2"
	.byte 0
/* end data */

.data
.align 8
str7:
	.ascii "u256 ok"
	.byte 0
/* end data */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	sub $152, %rsp
	pushq %rbx
	pushq %r12
	pushq %r13
	leaq -128(%rbp), %rsi
	leaq str1(%rip), %rdi
	callq ferret_i128_from_string_ptr
	movl $16, %edx
	leaq -128(%rbp), %rsi
	leaq -144(%rbp), %rdi
	callq ferret_memcpy
	leaq -96(%rbp), %rsi
	leaq str2(%rip), %rdi
	callq ferret_i128_from_string_ptr
	movl $16, %edx
	leaq -96(%rbp), %rsi
	leaq -112(%rbp), %rdi
	callq ferret_memcpy
	leaq -64(%rbp), %rdx
	leaq -112(%rbp), %rsi
	leaq -144(%rbp), %rdi
	callq ferret_i128_sub_ptr
	movl $16, %edx
	leaq -64(%rbp), %rsi
	leaq -80(%rbp), %rdi
	callq ferret_memcpy
	leaq -32(%rbp), %rsi
	leaq str3(%rip), %rdi
	callq ferret_i128_from_string_ptr
	leaq -16(%rbp), %rdx
	leaq -80(%rbp), %rsi
	leaq -32(%rbp), %rdi
	callq ferret_i128_sub_ptr
	movl $16, %edx
	leaq -16(%rbp), %rsi
	leaq -48(%rbp), %rdi
	callq ferret_memcpy
	leaq -144(%rbp), %rsi
	leaq -48(%rbp), %rdi
	callq ferret_i128_lt_ptr
	cmpl $0, %eax
	jz .Lbb2
	leaq str4(%rip), %rdi
	callq ferret_io_Println
.Lbb2:
	subq $32, %rsp
	movq %rsp, %rdi
	subq $32, %rsp
	movq %rsp, %rsi
	movq %rsi, %r12
	movq %rdi, %rbx
	leaq str5(%rip), %rdi
	callq ferret_u256_from_string_ptr
	movq %r12, %rsi
	movq %rbx, %rdi
	movl $32, %edx
	movq %rdi, %rbx
	callq ferret_memcpy
	movq %rbx, %rdi
	subq $32, %rsp
	movq %rsp, %rbx
	subq $32, %rsp
	movq %rsp, %rsi
	movq %rsi, %r13
	movq %rdi, %r12
	leaq str6(%rip), %rdi
	callq ferret_u256_from_string_ptr
	movq %r13, %rsi
	movq %r12, %rdi
	movl $32, %edx
	movq %rdi, %r12
	movq %rbx, %rdi
	callq ferret_memcpy
	movq %r12, %rdi
	subq $32, %rsp
	movq %rsp, %r12
	subq $32, %rsp
	movq %rsp, %rsi
	movq %rsi, %rdx
	movq %rsi, %r13
	movq %rbx, %rsi
	callq ferret_u256_add_ptr
	movq %r13, %rsi
	movl $32, %edx
	movq %r12, %rdi
	callq ferret_memcpy
	movq %rbx, %rsi
	subq $32, %rsp
	movq %rsp, %rdi
	subq $32, %rsp
	movq %rsp, %r13
	movq %r13, %rdx
	movq %rdi, %rbx
	movq %r12, %rdi
	callq ferret_u256_mul_ptr
	movq %r13, %rsi
	movq %rbx, %rdi
	movl $32, %edx
	movq %rdi, %rbx
	callq ferret_memcpy
	movq %r12, %rsi
	movq %rbx, %rdi
	callq ferret_u256_gt_ptr
	cmpl $0, %eax
	jz .Lbb4
	leaq str7(%rip), %rdi
	callq ferret_io_Println
.Lbb4:
	movl $0, %eax
	movq %rbp, %rsp
	subq $176, %rsp
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
/* end function main */

