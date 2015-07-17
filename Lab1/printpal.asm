# NWEN 242 Lab 1
# Program: printstring 
# Author: David Barnett 300313764

.data
	str: .asciiz  "David Barnett 300313764" #replace with your name and birthday

.text
	la  $a0, str # load the address of the string
	addi $t0, $a0, 0 # starting address of array

	#TODO: find length of string, it's zero terminated, so loop until a zero
	addi $t1, $a0, 24 # initialize loop counter to array end position (replace with your str length)
    addi $t3, $t0, 0  # Load starting index to iterator
    
loopfwd:   lb $a0, 0($t3) # load a single character
	li $v0, 11 # specify print character service
	syscall # print 
	addi $t3, $t3, 1 # add 1 to the loop index
	blt $t3, $t1, loopfwd # continue if not at string length

loopbck: lb $a0, 0($t3) # load a single character
	li $v0, 11 # specify print character service
	syscall # print 
	subi $t3, $t3, 1 # sub 1 to the loop index
	bge $t3, $t0, loopbck # continue if not less than zero

	li $v0, 10 # system call for exit
	syscall # we are out of here.
