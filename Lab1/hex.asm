.data
	hexchar: .asciiz "0123456789ABCDEF"
	ask: .asciiz "Enter a decimal number: "
	ans: .asciiz "The number in hex representation is: 0x"
    int: .word 112385
.text

main:
	# ask for input
	la $a0, ask
	li $v0, 4
	syscall
	li $v0, 5
	syscall
	move $s1, $v0
	la $a0, ans
	li $v0, 4
	syscall
	
# $a0 word 
hex:
	addi $t0, $zero, 28 # index
	
	
hex_loop:
	# get 4-bits
	srlv $t1, $s1, $t0
	andi $t1, $t1, 15 # get only the 1st 4 bits
	
	lb $a0, hexchar($t1)
	li $v0, 11
	syscall
	
	subi $t0, $t0, 4
	bgez $t0, hex_loop
	# print character