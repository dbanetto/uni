% SWEN304 - Assignment 4 
% David Barnett (300313764)

# Question 1 - Functional Dependencies and Normal Forms

## A

 1. $A \rightarrow B$ - holds for the instance, all values of $A$ map to a single value of $B$

 2. $B \rightarrow A$ - Does not hold, not all values of $B$ map to a single value of $A$, e.g. $2 \rightarrow 1$ and $2 \rightarrow 4$

 3. $B \rightarrow C$ - holds for the instance, all values of $B$ map to a single value of $C$

## B

**Note: the key is $AB$**

 1. $F = \{ AB \rightarrow C, C \rightarrow D \}$

1NF holds - no nested relations

2NF holds - no functional dependency's left hand side is a subset of the key

3NF does not holds - there exists a transitive relation between functional dependencies
between $AB \rightarrow C$ and $C \rightarrow D$ which turns into $AB \rightarrow D$ via
the transitive rule.

The functional dependencies are in the 2NF as it is the highest normal form that holds.

 2. $F = \{ AB \rightarrow D, B \rightarrow C \}$

1NF holds - no nested relations

2NF does not hold - the functional dependency $B \rightarrow C$'s left hand side ($B$) is a subset of the key $AB$.

The functional dependencies are in the 1NF as it is the highest normal form that holds.

 3. $F = \{ AB \rightarrow C, AB \rightarrow D \}$

1NF holds - no nested relations.

2NF holds - no functional dependency's left hand side is a subset of the key.

3NF holds - no functional dependency is transitive to another dependency.

BCNF holds - all functional dependencies in the set has there left hand side is a super key  of or equal to $AB$ 

The functional dependencies are in the BCNF as it is the highest normal form that holds.

 4. $F = \{ AB \rightarrow C, C \rightarrow B \}$

1NF holds - no nested relations.

2NF holds - no functional dependency's left hand side is a subset of the key.

3NF holds - no functional dependency is transitive to another dependency.

BCNF does not holds - not functional dependencies in the set has there left hand side are a super key  of or equal to $AB$,
in this case $C \rightarrow B$'s left hand side is not a super key of $AB$.

The functional dependencies are in the 3NF as it is the highest normal form that holds.

# Question 2 - Minimal Cover of a set of Functional Dependencies

Initial set $F = \{ A \rightarrow B, B \rightarrow C, CD \rightarrow A , AC \rightarrow D \}$

## 1. Decomposition of right hand side

$F = \{ A \rightarrow B, B \rightarrow C, CD \rightarrow A , AC \rightarrow D \}$

No decomposition required.

## 2. Reduce Redundant Attributes

Only two functional dependencies have more than once attribute on the
left hand side to the checked if they can be reduced,
$CD \rightarrow A$ and $AC \rightarrow D$.

Checking $CD \rightarrow A$

$(CD - C)^{+}_{F - \{ CD \rightarrow A \}} = (D)^{+}_{F - \{ CD \rightarrow A \}} = D$

Closure of attribute does not contain the removed attribute, $C$ from $CD \rightarrow A$ 
is not redundant.

$(CD - D)^{+}_{F - \{ CD \rightarrow A \}} = (C)^{+}_{F - \{ CD \rightarrow A \}} = C$

Closure of attribute does not contain the removed attribute, $C$ from $CD \rightarrow A$ 
is not redundant.


Checking $AC \rightarrow D$

$(AC - A)^{+}_{F - \{ AC \rightarrow D \}} = (C)^{+}_{F - \{ AC \rightarrow D \}} = C$

Closure of attribute does not contain the removed attribute, $A$ from $AC \rightarrow D$ 
is not redundant.

$(AC - C)^{+}_{F - \{ AC \rightarrow D \}} = (A)^{+}_{F - \{ AC \rightarrow D \}} = ABC$

Does include the removed attribute, $C$ from $AC \rightarrow D$ is redundant and can be 
removed.

The resulting functional dependencies is:

$F = \{ A \rightarrow B, B \rightarrow C, CD \rightarrow A , A \rightarrow D \}$

## 3. Remove Redundant Functional Dependencies

# Question 3 - Normalization

## A)

### 1.

### 2.

### 3.

## B)

### 1.

### 2.

### 3.

## C)

### 1.

### 2.

### 3.

# Question 4 - Enhanced Entity Relationship

# Question 5 - Mapping EER to Relation Data Model
