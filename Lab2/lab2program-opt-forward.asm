addi $2, $0, 0
addi $9, $0, 12 // moved to be away from lw 12($0)
sw   $9, 12($0) // moved to be away from lw 12($0)
sw   $2, 0($0)
addi $3, $0, 2
lw   $5, 12($0) // Moved, does not need to run every loop is const
addi $4, $0, 100 // Moved, does not need to run every loop is const
uncon:
add $1, $2, $3
slt  $6, $1, $4
sw   $1, 0($0) // Moved to remove a NOP
beq  $6, $0, fin
nop
nop
nop
lw   $8, 0($0) // Moved to remove nop
add  $2, $2, $2
add  $3, $3, $3
beq  $8, $0, fin
nop
nop
nop
lw   $7, 0($5)
nop
add  $7, $3, $7
beq  $0, $0, uncon
nop
nop
nop
fin:
END
