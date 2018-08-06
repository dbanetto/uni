addi $2, $0, 3
nop
nop
nop
sw   $2, 0($0)
addi $3, $0, 2
addi $9, $0, 12
nop
nop
nop
sw   $9, 12($0)
nop
nop
uncon:
add  $1, $2, $3
lw   $5, 12($0)
nop
nop
sw   $1, 0($0)
addi $4, $0, 100
nop
nop
nop
slt  $6, $1, $4
nop
nop
nop
beq  $6, $0, fin
nop
nop
nop
add  $2, $2, $2
add  $3, $3, $3
lw   $8, 0($0)
nop
nop
nop
beq  $8, $0, fin
nop
nop
nop
lw   $7, 0($5)
nop
nop
nop
add  $7, $3, $7
beq  $0, $0, uncon
nop
nop
nop
fin:
END
