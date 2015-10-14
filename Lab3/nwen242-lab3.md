% NWEN242 Lab 3
% David Barnett (300313764)

# 3 - Preparatory questions

## A

Cache memory is a level between the main memory and the CPU allowing for
values that are used over and over again to be stored in a way that is much
faster than fetching from the main memory, this can also include memory that
is close to each other, such as in an array, if the block size of the cache allows it.

## B

The cache memory improves the performance as it reduces the time needed to fetch
from the memory, be it an instruction for a piece of data, with fewer cycles waitnig
for the operation to complete.

## C

Cache hits are attempting to fetch a block of data and the cache currently
holds the value so it a cache hit and returns it. A cache miss is when the cache
does not have the memory block and must go up the memory hierarchy to a slower memory.

## D

When the processor's request cannot be satisfied from the cache it is counted as a miss
and the request is then sent to the next layer in the memory hierarchy, be it main memory
or a higher level of cache.

## E

The types of caches considered in lectures were primarily D-cache, data cache,
with mentions of I-cache, instruction cache.

## F

The block size influences the processors performance through having a larger block size
allows for greater advantage for spacial locality but at the cost of the temporal locality which relies more on having more block indexes.

## G

Byte offset: 00000000 00000000 11000000 010001**[10]**

Word offset: 00000000 00000000 11000000 0100**[01]**10

Slot number / Index: 00000000 00000000 11000000 0**[100]**0110

Tag: **[00000000 00000000 11000000 0]**1000110

## H

00000000 00000011 00001001 10000000

00000000 00000011 00001001 10000100

00000000 00000011 00001001 10001000

00000000 00000011 00001001 10001100


## I

Write-Through is writing the changes to the cache and to a buffer that will later update the main memory.

Write-back is on a data-write only update the cache and mark it as a 'dirty' block that will be written back to the memory when it is replaced in cache. Can also incorporate
Write-through's  technique of using a write buffer to stager out writes to main memory.

\pagebreak

# 4.1 Mapping functions

## A

### Results

#### Registers

+----------+------------+
| Register | value      |
+==========+============+
| R2 (v0)  | 0x100004a0 |
+----------+------------+
| R5 (a1)  | 0x00000002 |
+----------+------------+
| R6 (a2)  | 0x0000000c |
+----------+------------+

> *Note: all other registers were zero so were omitted.*
> *The stack pointer and global pointer are also omitted*

#### Instruction Cache

+----------+----------+
| Accesses | 53       |
+----------+----------+
| Hits     | 42       |
+----------+----------+
| Hit Rate | 0.792453 |
+----------+----------+

#### Data Cache

+----------+----------+
| Accesses | 8        |
+----------+----------+
| Hits     | 6        |
+----------+----------+
| Hit Rate | 0.75     |
+----------+----------+

##### Data Cache Misses

+------------+---+
| Compulsory | 2 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

\pagebreak

## B

### Data Cache

| Set | Valid | LRU | Tag    | Data       |
|-----|-------|-----|--------|------------|
| 0   | 1     | 0   | 400012 | 1, 1, 1 ,1 |
| 1   | 1     | 0   | 400012 | 2, 2, 2, 2 |

The elements of `Array_A` are mapped to the data cache through breaking down their binary memory address.
For example the first piece of data in set 0 has the memory address is 0001 0000 0000 0000 0000 0100 1000 0000, in binary format,
and it is broken down to to values used in the memory cache: byte offset (bits 0 to 1), word offset (bits 2 to 3), index (bits 4 to 8) and tag (bits 9 to 31).
Since the cache has 16 bytes, or 4 words, per block the first four words of `Array_A` is stored in set 0 as they are all a part of the same 4 word block that the cache holds. The second half of `Array_A` is in set 1 as the index portion of the address was different by one to the first half of the array.

## C

### Instruction Cache

| Set | V | Tag  | Instruction         |
|-----|---|------|---------------------|
| 0   | 1 | 8000 | lui \$1,  4096      |
| 1   | 1 | 8000 | ori \$1,  \$1, 1152 |
| 2   | 1 | 8000 | ori \$6,  \$0, 0    |
| 3   | 1 | 8000 | ori \$4,  \$0, 8    |
| 4   | 1 | 8000 | lw \$5,   0(\$2)    |
| 5   | 1 | 8000 | add \$6,  \$6, \$5  |
| 6   | 1 | 8000 | addi \$2, \$2, 4    |
| 7   | 1 | 8000 | addi \$4, \$4, -1   |
| 8   | 1 | 8000 | slt \$1,  \$0, \$4  |
| 9   | 1 | 8000 | bne \$1,  \$0, -20  |
| 10  | 1 | 8000 | NULL                |


The contents of the instruction cache are obtained while executing the program.
As the program executes the program counter, PC, increments to point to the current
instruction in memory. During an instruction fetch the CPU will check the cache before going
to the next level in the instruction memory hierarchy and update the cache with this value after
it receives it. The instruction cache is laid out in the same way as the data cache, but in this
case a block only contains one word, using parts of the address to be the tag and index for mapping in
the cache.

## D

### Results

#### Registers


| Register   | value           |
|------------|-----------------|
| R2 (v0)    | 0x100004a0      |
| R2 (v1)    | 0x100004c0      |
| R5 (a1)    | 0x00000002      |
| R6 (a2)    | 0x00000028 (40) |
| R8 (t1)    | 0x0000000c      |

> *Note: all other registers were zero so were omitted.*
> *The stack pointer and global pointer are also omitted*

#### Instruction Cache

+----------+----------+
| Accesses | 79       |
+----------+----------+
| Hits     | 63       |
+----------+----------+
| Hit Rate | 0.797468 |
+----------+----------+

#### Data Cache

+----------+----------+
| Accesses | 16       |
+----------+----------+
| Hits     | 12       |
+----------+----------+
| Hit Rate | 0.75     |
+----------+----------+

##### Data Cache Misses

+------------+---+
| Compulsory | 4 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

### Data Cache

| Set | Valid | LRU | Tag    | Data       |
|-----|-------|-----|--------|------------|
| 0   | 1     | 0   | 400012 | 1, 1, 1 ,1 |
| 1   | 1     | 0   | 400012 | 2, 2, 2, 2 |
| 2   | 1     | 0   | 400012 | 3, 3, 3 ,3 |
| 3   | 1     | 0   | 400012 | 4, 4, 4, 4 |

### Instruction Cache

| Set | V | Tag  | Instruction         |
|-----|---|------|---------------------|
| 0   | 1 | 8000 | lui \$1,  4096      |
| 1   | 1 | 8000 | ori \$2,  \$1, 1152 |
| 2   | 1 | 8000 | lui \$1,  4096      |
| 3   | 1 | 8000 | ori \$3,  \$1, 1152 |
| 4   | 1 | 8000 | ori \$6,  \$0, 0    |
| 5   | 1 | 8000 | ori \$4,  \$0, 8    |
| 6   | 1 | 8000 | lw \$5,   0(\$2)    |
| 7   | 1 | 8000 | lw \$8,   0(\$3)    |
| 8   | 1 | 8000 | add \$6,  \$6, \$5  |
| 9   | 1 | 8000 | add \$6,  \$6, \$8  |
| 10  | 1 | 8000 | addi \$2, \$2, 4    |
| 11  | 1 | 8000 | addi \$3, \$3, 4    |
| 12  | 1 | 8000 | addi \$4, \$4, -1   |
| 13  | 1 | 8000 | slt \$1,  \$0, \$4  |
| 14  | 1 | 8000 | bne \$1,  \$0, -20  |
| 15  | 1 | 8000 | NULL                |

> *Code used included as `map-4-1-d.asm`*

<!-- FIXME: needs a bit more work here -->

The elements of `Array_A` and `Array_B` are mapped to the memory cache using direct mapping with the memory address as the basis to map the words to cache.
The data cache holds four values as the block size is of size 16 bytes or 4 words.


# 4.2 Temporal and spatial locality

## A


###  16 byte block

+----------+------+
| Accesses | 16   |
+----------+------+
| Hits     | 12   |
+----------+------+
| Hit Rate | 0.75 |
+----------+------+

#### Misses

+------------+---+
| Compulsory | 4 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+


### 8 byte block

+----------+-----+
| Accesses | 16  |
+----------+-----+
| Hits     | 8   |
+----------+-----+
| Hit Rate | 0.5 |
+----------+-----+

#### Misses

+------------+---+
| Compulsory | 8 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

### 4 byte block

+----------+----+
| Accesses | 16 |
+----------+----+
| Hits     | 0  |
+----------+----+
| Hit Rate | 0  |
+----------+----+

#### Misses

+------------+----+
| Compulsory | 16 |
+------------+----+
| Conflict   | 0  |
+------------+----+
| Capacity   | 0  |
+------------+----+

The hit rate of 0.75% can be achieve with the block size of 16 bytes.
This is because when the first and fifth element of the arrays is accessed
when loading into the memory cache the next three elements are also loaded so
the next three `lw`s will result in hits due to spacial locality of the words in memory.

Based on the data recored it can be proven that having a larger block size can increase hit
rate due to the close locations of the elements in the array in memory.
This is evidence to a positive relationship between the hit rate and block size in a spacial local
operation such as iterating through an array.

## B

> *Code used included as `loop-4-2-b.asm`*

### N = 0

+----------+-----+
| Accesses | 16  |
+----------+-----+
| Hits     | 12  |
+----------+-----+
| Hit Rate | 0.75|
+----------+-----+

#### Misses

+------------+---+
| Compulsory | 4 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

### N = 5

+----------+-----+
| Accesses | 80  |
+----------+-----+
| Hits     | 76  |
+----------+-----+
| Hit Rate | 0.95|
+----------+-----+

#### Misses

+------------+---+
| Compulsory | 4 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

### N = 10

+----------+------+
| Accesses | 160  |
+----------+------+
| Hits     | 76   |
+----------+------+
| Hit Rate | 0.975|
+----------+------+

#### Misses

+------------+---+
| Compulsory | 4 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

### N = 100

+----------+-------+
| Accesses | 1600  |
+----------+-------+
| Hits     | 1596  |
+----------+-------+
| Hit Rate | 0.9975|
+----------+-------+

#### Misses

+------------+---+
| Compulsory | 4 |
+------------+---+
| Conflict   | 0 |
+------------+---+
| Capacity   | 0 |
+------------+---+

The results shows that there are only ever four misses no matter how many times the loop runs in this program.
This is because the four misses are loading the four cache blocks of data to be reuse over and over again.
The repeated use of the memory is temporal locality.
The affect of temporal locality, using the same memory soon after in respect to time, on the results is
strong as the results show only four misses and increasing hit rates as the number of times the same memory
locations are hit over and over again. From the results we can conclude that using the same block over memory
over and over again results in increased hit rates and thus performance.

# 4.3 Replacement algorithms

## A

### Contents of set 0

0x40, 0x41, 0x42, 0x43

The contents of set 0 is the values 64 to 67 (0x40 to 0x43) because the
total cache size is 256 bytes or 64 words so as the array of size 512 bytes
or 128 words is loaded the index values wrapped around and started to override
previous blocks in the cache with the new blocks.


## B

### Changes to set 0

|Change # | LRU | Tag W1 | Data W1        | LRU | Tag W2 | Data W2
|---------|-----|--------|----------------|-----|--------|------------------
|0        | 0   | 400000 | 0 , 1,  2,  3  | 0   |        |
|1        | 1   | 400000 | 0 , 1,  2,  3  | 0   | 400001 | 16,  17,  18,  19
|3        | 0   | 400002 | 32, 33, 34, 35 | 1   | 400001 | 16,  17,  18,  19
|4        | 1   | 400002 | 32, 33, 34, 35 | 0   | 400003 | 48,  49,  50,  51
|5        | 0   | 400004 | 64, 65, 66, 67 | 1   | 400003 | 48,  49,  50,  51
|6        | 1   | 400004 | 64, 65, 66, 67 | 0   | 400005 | 80,  81,  82,  83
|7        | 0   | 400006 | 96, 97, 98, 99 | 1   | 400005 | 80,  81,  82,  83
|8        | 1   | 400006 | 96, 97, 98, 99 | 0   | 400007 | 112, 113, 114, 115

The LRU values indicate the least recently used element of the 2 way cache, so the higher the LRU the least recently used the
block is.
This is  used to determine which block in cache to replace once a new block is needed.
On a replacement the LRU of the new block is set to zero and all other valid blocks increment LRU by 1.
Accessing a block already in the cache will do the same.
The changes shown by set 0 reflect this as when the LRU is 1 on the next cache miss the data in that index of the set
is replaced with a new value.

## C

### Changes to set 0


|Change # | FIFO| Tag W1 | Data W1        | FIFO| Tag W2 | Data W2
|---------|-----|--------|----------------|-----|--------|------------------
|0        | 0   | 400000 | 0 , 1,  2,  3  | 0   |        |
|1        | 1   | 400000 | 0 , 1,  2,  3  | 0   | 400001 | 16,  17,  18,  19
|3        | 0   | 400002 | 32, 33, 34, 35 | 1   | 400001 | 16,  17,  18,  19
|4        | 1   | 400002 | 32, 33, 34, 35 | 0   | 400003 | 48,  49,  50,  51
|5        | 0   | 400004 | 64, 65, 66, 67 | 1   | 400003 | 48,  49,  50,  51
|6        | 1   | 400004 | 64, 65, 66, 67 | 0   | 400005 | 80,  81,  82,  83
|7        | 0   | 400006 | 96, 97, 98, 99 | 1   | 400005 | 80,  81,  82,  83
|8        | 1   | 400006 | 96, 97, 98, 99 | 0   | 400007 | 112, 113, 114, 115

Using the FIFO, first in first out, memory cache strategy the content of set 0
update throughout the program. As the array iterated through the values in set 0
changed. The oldest block in the set was replaced by the new block received after the
miss.

\pagebreak

# 4.4 Accessing at different strides

## A

See `replace-4-4.asm`

## B

Stride | N | Hit rate | Compulsory Miss | Conflict Miss | Capacity Miss
-------|---|----------|-----------------|---------------|--------------
1      | 1 | 75%      | 32              | 0             | 0
1      | 2 | 75%      | 32              | 0             | 32
1      | 4 | 75%      | 32              | 0             | 96
2      | 1 | 50%      | 32              | 0             | 0
2      | 2 | 50%      | 32              | 0             | 32
2      | 4 | 50%      | 32              | 0             | 96
4      | 1 | 0%       | 32              | 0             | 0
4      | 2 | 0%       | 32              | 0             | 32
4      | 4 | 0%       | 32              | 0             | 96
8      | 1 | 0%       | 16              | 0             | 0
8      | 2 | 0%       | 16              | 16            | 0
8      | 4 | 0%       | 16              | 48            | 0
16     | 1 | 0%       | 8               | 0             | 0
16     | 2 | 0%       | 8               | 8             | 0
16     | 4 | 0%       | 8               | 24            | 0
32     | 1 | 0%       | 4               | 0             | 0
32     | 2 | 50%      | 4               | 0             | 0
32     | 4 | 75%      | 4               | 0             | 0
64     | 1 | 0%       | 2               | 0             | 0
64     | 2 | 50%      | 2               | 0             | 0
64     | 4 | 75%      | 2               | 0             | 0


## C

<!-- TODO: write up here -->

The data recorded show a few trends.
With strides less than the word size, such as 1 to 4, showed a trend of decreasing hit rate.
This would be due to the stride using an ever decreasing amount of the block that would be hit
by the next iteration of the loop.
They also had more capacity misses as the total size of the blocks that are entered into the cache
in the iteration is larger than the cache size.
This is not present in the higher stride lengths, such as 8 and above, due to not every potential
block is loaded into the cache.
The later strides started to increase in hit rate as the blocked that are pulled to memory are smaller
than the set size. This allows for the cache to be loaded on the first loop and have no need afterwards
to update the memory cache for any times of the loop, where lower strides will still have capacity misses
because it cannot store the whole array in cache.
The biggest factors that plays in the data is the cache size, set size and block size as they all influence
if there is capacity issues only using one word in each set entry.

\pagebreak

# 4.5 Instruction cache

## A

The optimal performance of 10/12 or 83% is achieve by using 16 byte block sizes and using, direct mapping, 2 way, 4 way and fully associative mapping
with using either the FIFO or LRU algorithms.

## B

The settings used are suitable for the program.
The critical factor was the block size.
This was because the memory addresses used are not in the same
block in any of the block sizes expect 16 bytes (4 words) giving it a large advantage
with only having to miss once to get both of the words the program wants in one in the cache.
This advantage was agnostic to all other cache settings as the program does not access any other
memory addresses which the other settings help more to decrease hit rate.

