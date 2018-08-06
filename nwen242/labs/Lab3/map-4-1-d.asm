.data 0x10000480
    Array_A:
.word 1,1,1,1,2,2,2,2
    Array_B:
.word 3,3,3,3,4,4,4,4

.text
.globl __start
__start:
    la $2, Array_A  # Load Array_B start address
    la $3, Array_B  # Load Array_B start address
    li $6, 0        # sum=0
    li $4, 8        # number of elements
loop:
    lw $5, 0($2)    # Load Array_A[i] == $5
    lw $8, 0($3)    # Load Array_B[i] == $8
    add $6, $6, $5  #sum=sum+Array_A[i]
    add $6, $6, $8  #sum=sum+Array_B[i]
    addi $2, $2, 4  # next index of Array_A
    addi $3, $3, 4  # next index of Array_B
    addi $4, $4, -1 # Decrement count
    bgt $4, $0, loop
.end
