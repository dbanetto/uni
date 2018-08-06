# NWEN 242 Lab 1
# Program: printstring 
# Author: David Barnett 300313764

#Power
.data
	prompt: .asciiz "Enter the number: "
	mesg: .asciiz "When two is raised to that power, the answer is: "
	endl: .asciiz "\n"
.text
	main:
		la $a0, prompt # print the prompt
		li $2, 4
		syscall
		li $2, 5 # read an integer from the user
		syscall #  into $v0
	
	power:  
		li $t1, 1     # result
		move $t2, $v0 # power
		_pow:
			subi $t2, $t2, 1 # t2 - 1
			mulo $t1, $t1, 2 # t1 * 2
			bgtz $t2, _pow    # loop back if more powers (t1) to apply
		move $a0, $t1
	
	output:
		la $a0, mesg # print some text
		li $2, 4
		syscall
		li $2, 1 # print the answer
		move $a0 $t1
		syscall
		la $a0, endl # print return
		li $2, 4
		syscall
		li $2, 10 # let's get out of here.
		syscall