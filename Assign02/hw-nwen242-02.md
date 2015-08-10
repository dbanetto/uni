# NWEN242 Homework Assignment 2

## David Barnett ID: 300313764

### Question 1

#### Part A

Yes

#### Part B

No

### Question 2

```mips
    addi $t0, $zero, 0 # i = 0
ILOOP:
    slt $t3,$s0,$t0
    beq $t3, $zero, ILOOPEND # i < a
    addi $t1, $zero, 0 # i = 0
JLOOP:
    slt $t3,$s1,$t1
    beq $t3, $zero, JLOOPEND # j < b

    add $t3, $t1, $t2 # $t3 = i + j
    sll $t4, $t2, 2   # j * 4
    add $t4, $s2, $t4 # $t4 = &D[j * 4]
    sw  $t3, 0($t4)   # D[j * 4] = $t3

    addi $t1, $t1, 1 # j++
    j JLOOP
JLOOPEND:

    j ILOOP
ILOOPEND:
```

### Question 3

Assuming `a = $a0` `b = $a1` `c = $a2` `d = $a3`

```mips
# TODO Save return pointer to stack
# call function
# restore stack pointer & rp
# return
```

### Question 6

$$XOR = A * \bar{B} + \bar{A} * B $$
