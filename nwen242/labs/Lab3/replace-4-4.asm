.data 0x10000000
    Array_A:
        .word 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127 # 128 elements in total
    .text
    .globl __start
__start:
    li $8, 2 # N = $8 = iterations of external loop
    li $v0, 5 # Ask user for int
    syscall
    move $3, $v0 # $3 = stride in elements
    li $6, 0 # $6 = sum
    sll $9, $3, 2 # $9 = stride in bytes = $3x4
ext_loop:
    li $5, 128 # $5 = number of elements of Array_A
    li $4, 0   # $4 = index
int_loop:
    lw $7, Array_A($4)
    add $6, $6, $7
    add $4, $4, $9 # address of next element
    sub $5, $5, $3
    bgt $5, $0, int_loop
    addi $8, $8, -1
    bgt $8, $0, ext_loop
.end

