.data
	endl: .asciiz "\n"

.text

main:


	addi $a0, $zero, 64
	jal F1 # Jump to F1 & set $ra to here
	li $2, 10 # exit
	syscall
	# F1 takes one parameter in $a0

F1:
	blt $a0, 1, BaseCase # handle the base case of the recursion

	# store relevant registers before jump
	subi $sp, $sp, 8 # move stack pointer backwards
	sw $a0, 4($sp)   # store  arg
	sw $ra, 8($sp)   # store return pointer

	div $a0, $a0, 2  # set the right parameters for the recursion
	jal F1           # perform the recursive call

	# reset data after return from recursion
	lw $a0, 4($sp) # restore arg
	lw $ra, 8($sp) # restore return pointer
	addi $sp, $sp, 8 # move stack pointer forward


	li $2, 1 # load print
	syscall  # print parameter already in $a0
	move $t0, $a0 # copy parameter
	la $a0, endl # print end of line
	li $2, 4
	syscall
	move $a0, $t0 # copy parameter back
	jr $ra

BaseCase:
	jr $ra # return to the address given in register $ra
