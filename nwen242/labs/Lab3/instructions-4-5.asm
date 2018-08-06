.data 0x10000000
    Base:
    .word 0, 10, 20
.text
.globl __start
__start:
    la $9, Base    # load Base array pointer to $9
    addi $3, $0, 3 # 3 ?
    addi $4, $0, 4 # 4 ?
    addi $6, $0, 3 # number of loops
loop:
    addi $5, $5, 1 # i++
    lw $8, 4($9)   # $8 = 10
    lw $2, 8($9)   # $2 = 20
    add $8, $8, $3 # 10 + 3 + i
    sw $8, 36($9)  # Base[9] = 10 + 3 + i
    add $2, $2, $4 # $2 = 20 + 4 + i
    sw $2, 40($9)  # Base[10] = 20 + 4 + i
    addi $3, $3, 1 # 3 + i
    addi $4, $4, 1 # 4 + i
    bne $5, $6, loop # $5 != $6
.end
