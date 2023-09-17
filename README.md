## Installation

```bash
cabal init
``` 

```bash
cabal build
cabal run abstract-algeba
``` 

## Problems


### Sets

1. Assume you have a proper definition for integers. Create a well-defined set of rational numbers.

Express this: `Q = { a / b : a ∈ Z, b ∈ Z, b ≠ 0 }` in Haskell

> **Answer**

```bash
cabal run definition-of-set
```

### Subsets

2. Define the subset relationship between integers, rational numbers, real numbers, and complex numbers.

> **Answer**

Integers and Rational Numbers:
- Integers are a subset of rational numbers.
- All integers can be represented as rational numbers where the denominator is 1.

Rational Numbers and Real Numbers:
- Rational numbers are a subset of real numbers.
- Real numbers include both rational numbers and irrational numbers.


Integers and Real Numbers:
- Integers are a subset of real numbers.
- All integers can be represented as real numbers.

Real Numbers and Complex Numbers:
- Real numbers are a subset of complex numbers.
- Complex numbers include both real numbers (where the imaginary part is zero) and purely imaginary numbers (where the real part is zero).

To visualize this relationship:

```
                       Complex Numbers
                            |
                       Real Numbers
                         /     \
         Rational Numbers       Irrational Numbers
              |
           Integers

```

- Complex numbers encompass all other number systems.
- Real numbers encompass rational numbers and irrational numbers.
- Rational numbers encompass integers.
- Integers are a subset of both rational numbers and real numbers.

In summary, the relationships among these number systems form a hierarchical structure where each system contains or overlaps with others, as described above.

Run to check if the given set is subset of another:

```bash
cabal run sub-set
```

> **Note**💡

>  We will run into infinite computation if we give the full list of all data types eg. Real Numbers

3. Define the relationship between the set of transcendental numbers and the set of complex numbers in terms of subsets. Is it a proper subset?

> **Answer**

The set of complex numbers, denoted by C, includes all numbers of the form a + bi, where a and b are real numbers, and i is the imaginary unit, which is defined as the square root of -1.

The set of transcendental numbers, denoted by T, consists of real or complex numbers that are not algebraic. An algebraic number is a root of a non-zero polynomial equation with integer coefficients. In other words, a transcendental number cannot be a solution to any algebraic equation with integer coefficients.

The relationship between the set of transcendental numbers and the set of complex numbers is such that:

`T ⊂ C`

This means that the set of transcendental numbers is a proper subset of the set of complex numbers. In other words, all transcendental numbers are complex numbers, but not all complex numbers are transcendental. The complex numbers include both transcendental numbers and algebraic numbers, which are solutions to algebraic equations.

```bash
cabal run transcendental-number
```

### Cardinality

4. Using the formal definition of equality, show that if two finite sets have different cardinality, they cannot be equal. (Demonstrating this for infinite sets is a little trickier, so we skip that).

> **Answer**

To start, let's consider two finite sets A and B with different cardinalities, and we want to show that A ≠ B.

Formal Definition of Equality for Sets: Two sets X and Y are equal (denoted as X = Y) if and only if every element of X is an element of Y and every element of Y is an element of X.

Now, let's proceed with the proof:

Assumption: Let A and B be two finite sets with different cardinalities, i.e., `|A| ≠ |B|.`

Proof by Contradiction: We assume that A = B and then derive a contradiction.

Assume A = B.

Since A = B, by the formal definition of equality, every element of A is an element of B and vice versa.

Now, let's consider the cardinality of A. We know that A has |A| elements.
Similarly, since A = B, B also has |A| elements (because every element of A is an element of B).

However, we assumed that |A| ≠ |B|, which means A and B have different cardinalities.

This contradiction arises from the assumption that A = B.

Therefore, our initial assumption that A = B must be false.

**Conclusion**: Since we have shown that the assumption A = B leads to a contradiction, and we know that A ≠ B from our initial assumption about their cardinalities, we can conclude that if two finite sets have different cardinalities, they cannot be equal.

This proof demonstrates that if two finite sets have different cardinalities, they cannot be equal according to the formal definition of equality for sets.

```bash
cabal run cardinality
```

### Cartesian product

5. We can define a set such that every element from one set is one part of an ordered pair with an element from another set. For example, if `A = {1,2,3}` and `B = {x, y, z}`, `A × B = {(1, x), (1, y), (1, z), (2, x), …, (3, z)}` The result of a cartesian product is still a set: a set of ordered pairs. Cartesian produc


ts are not commutative. Compute the cartesian product of B × A 

> **Answer**

```bash
cabal run cartesian_1
```

6. Compute the cartesian product of {1,2,3,4} and {3,6,9,12} (in that order). If you were to pick 4 particular ordered pairs from this, what arithmetic computation would that encode?

> **Answer**

```bash
cabal run cartesian_2
```

### Subsets of the cartesian product form a function

7. Define a mapping (function) from integers `n ∈ 1,2,3,4,5,6` to the set `{even, odd}`.

> **Answer**

```bash
cabal run mapping_1
```

8. Exercise: Take the cartesian product of the set of integers `0,1,2,…,8` and the polygons `triangle, square, pentagon, hexagon, heptagon, and octagon`. Define a mapping such that the integer maps to the number of sides on the shape. For example, the ordered pair `(4, □)` should be in the subset, but `(7,△)` should not be in the subset of the cartesian product.

> **Answer**

```bash
cabal run mapping_2
```

9. Define a mapping between positive integers and positive rational numbers (not the whole thing, obviously). It is possible to perfectly map the integers to rational numbers. Hint: draw a table to construct rational numbers where the columns are the numerators and the rows are the denominators.

> **Answer**

By using each cell in the table to represent a fraction in the form of numerator/denominator, you can achieve a perfect mapping between the positive integers and positive rational numbers.

```bash
     1   2   3   4   5   ...
     1  1/1 2/1 3/1 4/1 5/1 ...
     2  1/2 2/2 3/2 4/2 5/2 ...
     3  1/3 2/3 3/3 4/3 5/3 ...
     4  1/4 2/4 3/4 4/4 5/4 ...
     5  1/5 2/5 3/5 4/5 5/5 ...
     ... ... ... ... ... ...
```

In this table, each cell represents a rational number in the form of numerator/denominator. The numerator is the column number, and the denominator is the row number.

For example, the cell in the second row and third column (2/3) corresponds to the positive rational number that is obtained by dividing the numerator 2 by the denominator 3.

This table provides a perfect mapping between positive integers and positive rational numbers, as each positive rational number appears exactly once in the table.

Keep in mind that this mapping covers only the positive rational numbers, not all rational numbers (including negative rationals).

### Functions

10. Let set A be `{1,2,3}` and set B be `{x,y,z}`. Define a function from A to B that is well-defined, but not surjective and not injective.

Function f: A → B

f(1) = x
f(2) = x
f(3) = y

Explanation:

Well-Defined: Each element in set A is uniquely mapped to an element in set B according to the function f. For example, f(1) = x, f(2) = x, and f(3) = y.

Not Surjective (Not onto): The function is not surjective because not all elements in set B are mapped to by elements in set A. In this case, the element z from set B does not have any corresponding element in set A that maps to it. The function doesn't cover all elements in the codomain (set B).

Not Injective (Not one-to-one): The function is not injective because multiple elements in set A are mapped to the same element in set B. In this case, both 1 and 2 from set A are mapped to x in set B. Injections require that each element in the domain maps to a unique element in the codomain.

> **Answer**

```bash
cabal run function
```

### A “binary operator” in set theoretic terms

A binary operator is a function from `A × A → A.` Basically, we take every possible pair from A (the cartesian product of A with itself) and map it to A.

11. Pick a subset of ordered pairs that defines `a * b mod 3.` which is still in set `A`

> **Answer**

```bash
cabal run binary-operation_1
```

12. Here is an interesting fact about the relation between (A x A) and A above: if the binary operator is commutative, then the map from (A x A) to A cannot be injective if the cardinality of the set is 2 or greater.

Demonstrate the above statement is correct by reasoning from ((a, b), c) and ((b, a), c) and the definition of injective.

> **Answer**

Let's reason through this using the properties of injective functions and the fact that the binary operator is commutative. We will demonstrate that if the binary operator is commutative, then the map from (A x A) to A cannot be injective when the cardinality of set A is 2 or greater.

First, let's assume that the binary operator '*' is commutative. This means that for any elements a and b in set A, a * b = b * a.

Now, let's consider two distinct elements (a, b) and (b, a) from the Cartesian product (A x A), where a and b are distinct elements from set A. According to the commutative property of the binary operator, we have:

(a, b) = (b, a)

However, by definition, two ordered pairs are equal if and only if their corresponding elements are equal in order and value. Since a and b are distinct (a ≠ b), the ordered pairs (a, b) and (b, a) are distinct as well.

Now, let's consider the map f from (A x A) to A that maps ((a, b), c) to a * c. According to our assumptions, this map is well-defined since the binary operator is commutative.

If f were injective, it would mean that each unique element in the domain (A x A) is mapped to a unique element in the co-domain A. However, we have just shown that the ordered pairs (a, b) and (b, a) are distinct, but they are mapped to the same element a in the co-domain A due to the commutative property of the binary operator.

This violates the injective property, as injective functions require distinct elements in the domain to be mapped to distinct elements in the codomain.

Therefore, we have demonstrated that when the binary operator is commutative, the map from (A x A) to A cannot be injective when the cardinality of set A is 2 or greater. This is because distinct ordered pairs in the domain may be mapped to the same element in the codomain due to the commutativity of the binary operator.


13.  Define our set A to be the numbers 0,1,2,3,4 and our binary operator to be subtraction modulo 5. Define all the ordered pairs of A ⨉ A in a table, then map that set of ordered pairs to A.


### Semigroups

> **Answer**

```bash
cabal run binary-operation_3
```

14. Work out for yourself that concatenating “foo”, “bar”, “baz” in that order is associative. Remember, associative means `(A op B) op C = A op (B op C)`.

> **Answer**

```bash
cabal run semi-group_1
```

15. Give an example of `a magma` and `a semigroup`. The magma must not be a semigroup.

> **Answer**

**Magma Example:**

Let's Consider the set of positive integers {1, 2, 3, ...} as the magma's underlying set. The binary operation is defined as taking the average of two numbers:

- 2 ⊕ 3 = (2 + 3) / 2 = 2.5
- 4 ⊕ 2 = (4 + 2) / 2 = 3
- 5 ⊕ 5 = (5 + 5) / 2 = 5

This binary operation is not associative, as shown in the example below:

- (2 ⊕ 3) ⊕ 4 = (2.5) ⊕ 4 = 3.25
- 2 ⊕ (3 ⊕ 4) = 2 ⊕ (3.5) = 2.75

Since this binary operation is not associative, this example constitutes a magma that is not a semigroup.

**Semigroup Example:**

Let's consider the set of non-negative integers {1, 2, ...} as the semigroup's underlying set. The binary operation is defined as addition:

2 ⨁ 3 = 2 + 3 = 5
4 ⨁ 2 = 4 + 2 = 6
5 ⨁ 5 = 5 + 5 = 10
The binary operation of addition is associative:

(2 ⨁ 3) ⨁ 4 = (5) ⨁ 4 = 5 + 4 = 9
2 ⨁ (3 ⨁ 4) = 2 ⨁ (7) = 2 + 7 = 9
Since the binary operation is associative, this example constitutes a semigroup.

> **Note**💡

> Addition over positive integers without zero is a semigroup, but if you include zero, it becomes a monoid.

```bash
cabal run semi-group_2
```


### Monoids

16. Let our binary operator be the function min(a,b) over integers. Is this a magma, semigroup, or monoid? What if we restrict the domain to be positive integers (zero or greater)? What about the binary operator max(a,b) over those two domains?

> **Answer**

**Binary Operator: min(a, b)**

- **All Integers (Including Negative):** Semigroup
  - The binary operation `min(a, b)` is closed and associative for all integers, including negative integers. Therefore, it forms a semigroup.

- **Positive Integers (Zero or Greater):** Semigroup
  - The binary operation `min(a, b)` is closed and associative for positive integers (zero or greater). It also forms a semigroup.

- **Positive Integers (Zero or Greater) with Identity Element: Monoid**
  - The identity element for this case is `infinity (∞)`, as min(a, ∞) = min(∞, a) = a for any positive integer a. Therefore, over positive integers with an identity element, it forms a monoid.

**Binary Operator: max(a, b)**

- **All Integers (Including Negative):** Semigroup
  - The binary operation `max(a, b)` is closed and associative for all integers, including negative integers. It forms a semigroup.

- **Positive Integers (Zero or Greater):** Semigroup
  - The binary operation `max(a, b)` is closed and associative for positive integers (zero or greater). It forms a semigroup.

- **Positive Integers (Zero or Greater) with Identity Element: Monoid**
  - The identity element for this case is 0, as max(a, 0) = max(0, a) = a for any positive integer a. It forms a monoid over positive integers with an identity element.

In summary:

- **min(a, b):**
  - All Integers (Including Negative): Semigroup
  - Positive Integers (Zero or Greater): Semigroup
  - Positive Integers (Zero or Greater) with Identity Element: Monoid

- **max(a, b):**
  - All Integers (Including Negative): Semigroup
  - Positive Integers (Zero or Greater): Semigroup
  - Positive Integers (Zero or Greater) with Identity Element: Monoid

```bash
cabal run monoid_1
```


17. Let our set be all 3 bit binary numbers (a set of cardinality 8). Let our possible binary operators be bit-wise and, bit-wise or, bit-wise xor, bit-wise nor, bit-wise xnor, and bit-wise nand. Clearly this is closed because the output is a 3 bit binary number. For each binary operator, determine if the set under that binary operator is a magma, semigroup, or monoid.

> **Answer**

Let's analyze each binary operator in turn:

**Binary Operator: Bit-wise AND**

- **Closed:** The result of bit-wise AND between two 3-bit binary numbers is a 3-bit binary number. Hence, it is closed.

- **Associative:** Bit-wise AND is associative since the order of operations does not matter.

- **Identity Element:** There is no identity element (element such that a ⋆ e = e ⋆ a = a) in the set under bit-wise AND.

Overall, the set under bit-wise AND is a magma.

**Binary Operator: Bit-wise OR**

- **Closed:** The result of bit-wise OR between two 3-bit binary numbers is a 3-bit binary number. Hence, it is closed.

- **Associative:** Bit-wise OR is associative since the order of operations does not matter.

- **Identity Element:** There is no identity element (element such that a ⋆ e = e ⋆ a = a) in the set under bit-wise OR.

Overall, the set under bit-wise OR is a magma.

**Binary Operator: Bit-wise XOR**

- **Closed:** The result of bit-wise XOR between two 3-bit binary numbers is a 3-bit binary number. Hence, it is closed.

- **Associative:** Bit-wise XOR is associative since the order of operations does not matter.

- **Identity Element:** There is no identity element (element such that a ⋆ e = e ⋆ a = a) in the set under bit-wise XOR.

Overall, the set under bit-wise XOR is a magma.

**Binary Operator: Bit-wise NOR**

- **Closed:** The result of bit-wise NOR between two 3-bit binary numbers is a 3-bit binary number. Hence, it is closed.

- **Associative:** Bit-wise NOR is associative since the order of operations does not matter.

- **Identity Element:** There is no identity element (element such that a ⋆ e = e ⋆ a = a) in the set under bit-wise NOR.

Overall, the set under bit-wise NOR is a magma.

**Binary Operator: Bit-wise XNOR**

- **Closed:** The result of bit-wise XNOR between two 3-bit binary numbers is a 3-bit binary number. Hence, it is closed.

- **Associative:** Bit-wise XNOR is associative since the order of operations does not matter.

- **Identity Element:** There is no identity element (element such that a ⋆ e = e ⋆ a = a) in the set under bit-wise XNOR.

Overall, the set under bit-wise XNOR is a magma.

**Binary Operator: Bit-wise NAND**

- **Closed:** The result of bit-wise NAND between two 3-bit binary numbers is a 3-bit binary number. Hence, it is closed.

- **Associative:** Bit-wise NAND is associative since the order of operations does not matter.

- **Identity Element:** There is no identity element (element such that a ⋆ e = e ⋆ a = a) in the set under bit-wise NAND.

Overall, the set under bit-wise NAND is a magma.

In summary:

- Bit-wise AND: Magma
- Bit-wise OR: Magma
- Bit-wise XOR: Magma
- Bit-wise NOR: Magma
- Bit-wise XNOR: Magma
- Bit-wise NAND: Magma

None of these operators form semigroups or monoids since they do not have identity elements.


### Groups

18. Why can’t strings under concatenation be a group?

> **Answer**

Strings under concatenation cannot form a group because they lack the property of having an inverse element for each element in the set.

In a group, for every element "a" in the set, there must exist an inverse element "a'" such that the binary operation of "a" and "a'" results in the identity element. In the case of integers with addition, the identity element is zero, and the inverse of an integer "x" is its negation "-x".

However, when dealing with strings under concatenation, there is no clear definition of an inverse element for every string. Concatenation of two strings "s1" and "s2" results in a longer string "s1s2", and it's not possible to find a single string that, when concatenated with "s1" or "s2", will always result in an identity string.

For example, consider the string "abc". There is no single string "s'" that, when concatenated with "abc" or any other string, will always result in an empty string (identity element). Different strings have different lengths and content, making it impossible to define a single inverse string for all possible cases.

This lack of a consistent inverse element for each string is why strings under concatenation do not form a group. Groups require an inverse for every element, and the properties of string concatenation make it incompatible with the requirements of a group.

```bash
cabal run group-ex_1
```

19. Polynomials under addition satisfy the property of a group. Demonstrate this is the case by showing it matches the three properties that define a group.

Polynomials under addition do indeed satisfy the properties of a group. Let's demonstrate this by showing how they match the three defining properties of a group:

1. **Closure:** The sum of two polynomials is still a polynomial. If we have two polynomials \(p(x)\) and \(q(x)\), their sum \(p(x) + q(x)\) is also a polynomial.

2. **Associativity:** Polynomial addition is associative. For any three polynomials \(p(x)\), \(q(x)\), and \(r(x)\), we have \((p(x) + q(x)) + r(x) = p(x) + (q(x) + r(x))\).

3. **Identity Element:** The identity element for polynomial addition is the zero polynomial, denoted as \(0(x)\). For any polynomial \(p(x)\), we have \(p(x) + 0(x) = p(x)\) and \(0(x) + p(x) = p(x)\).

Now, let's show the third property, the existence of inverse elements:

4. **Inverse Element:** For every polynomial \(p(x)\), there exists an inverse polynomial \(-p(x)\) such that \(p(x) + (-p(x)) = 0(x)\) and \((-p(x)) + p(x) = 0(x)\).

Therefore, polynomials under addition satisfy the closure property, associativity property, identity element property, and inverse element property, making them a group with respect to the binary operation of addition.

```bash
cabal run group-ex_2
```

### Rings

A Ring is a set with two binary operators such that

under the first binary operator, the set is a abelian group

under the second binary operator, the set is a monoid

the second binary operator distributes over the first

Remember, a monoid does not have an inverse, but a group does.

We do not require the monoid to be commutative. If it is, we refer to this ring as an abelian ring.


To illustrate the statement “the second binary operator distributes over the first”, let our binary operators be □ and ☆, where □ is the first binary operator and ☆ is the second binary operator. The following must be true for the field:


`(a □ b) ☆ c = (a ☆ c) □ (b ☆ c)`

`c ☆ (a □ b) = (c ☆ a) □ (c ☆ b)`


Note how the first binary operator `□` appears in the parenthesis on the left hand, then we apply the second binary operator `☆`. That is what we mean by saying “the second binary operator distrbutes over the first.” We do not require `c □ (a ☆ b)` to distribute.

The following is not necessarily true of a ring:

`(a □ b) ☆ c = c ☆ (a □ b)`


### Rings

20. By a ring’s definition, why is the above statement not always true? What assumption is it making about the ring?

> **Answer**

The statement `(a □ b) ☆ c = c ☆ (a □ b)` is not always true in a ring because it assumes that the first binary operator `□` is commutative. In other words, it assumes that for any elements a and b in the set, the result of applying the operation `□` to a and b is the same as applying it to b and a, i.e., `a □ b = b □ a` .

In a general ring, the first binary operator `□` does not have to be commutative. Rings are defined to be sets with two binary operations, one for addition and one for multiplication (often denoted as + and *). While the multiplication operation (*) must satisfy associativity ((a * b) * c = a * (b * c)), it is not required to be commutative.

If the multiplication operation (*) in a ring is commutative (a * b = b * a for all a and b in the set), then the ring is referred to as a commutative ring. In this case, the statement `(a □ b) ☆ c = c ☆ (a □ b)` would indeed hold true because both the addition and multiplication operations are commutative.

However, the definition of a ring allows for non-commutative multiplication, and in such cases, the statement `(a □ b) ☆ c = c ☆ (a □ b)` may not be true because the order of multiplication matters, and distributivity is defined as shown in your original statement:

`(a □ b) ☆ c = (a ☆ c) □ (b ☆ c)`

`c ☆ (a □ b) = (c ☆ a) □ (c ☆ b)`

So, the assumption about the ring is that the multiplication operation (*) is not necessarily commutative, and distributivity is defined accordingly.


21. Use the definition of a ring to show that the trivial ring is in fact a ring

> **Answer**

To show that the trivial ring, which consists only of the element {0}, is indeed a ring, we need to demonstrate that it satisfies the defining properties of a ring. 

A ring is defined as a set equipped with two binary operations, usually denoted as addition (+) and multiplication (·), such that the following properties hold:

1. **Additive Abelian Group:** The set under addition forms an abelian (commutative) group. This means that addition is associative, has an identity element (0), and every element has an additive inverse.

2. **Multiplication Closure:** The set is closed under multiplication. For any elements a and b in the set, the product a · b is also in the set.

3. **Distributive Property:** Multiplication distributes over addition. For any elements a, b, and c in the set, the following property holds:
   a · (b + c) = (a · b) + (a · c) and (b + c) · a = (b · a) + (c · a)

Now, let's prove that the trivial ring satisfies these properties:

1. **Additive Abelian Group:**
   - **Associativity:** Since there is only one element, associativity of addition is trivial.
   - **Identity Element:** The identity element is 0, which is the only element in the set.
   - **Additive Inverse:** Since there is only one element, its additive inverse is itself (0).

2. **Multiplication Closure:** Since there is only one element (0) in the set, any product of elements in the set will also be 0, which is in the set.

3. **Distributive Property:**
   - For any elements a, b, and c in the set, we have:
     a · (b + c) = 0 · (0 + 0) = 0 = (0 · 0) + (0 · 0)
     (b + c) · a = (0 + 0) · 0 = 0 = (0 · 0) + (0 · 0)

Therefore, we've shown that the trivial ring satisfies all the properties of a ring. It's a ring consisting of only the element {0}, and its operations of addition and multiplication are defined in such a way that all ring properties are met.


22. Square matrices of real numbers under addition and multiplication is a ring. Demonstrate this to be the case. Think carefully about what the idenity elements are and whether an inverse always exists.

Note that matrix multiplication is not commutative in general. We only need to show it is a ring, not an abelian ring.

> **Answer**

1. **Additive Abelian Group:**
    - **Associativity:** Matrix addition is associative. For any square matrices A, B, and C of the same order, (A + B) + C = A + (B + C).
    - **Identity Element:** The identity element under addition is the zero matrix (a matrix where all elements are 0). For any square matrix A, A + 0 = 0 + A = A.
    - **Additive Inverse:** For every square matrix A, there exists an additive inverse (-A) such that A + (-A) = (-A) + A = 0. In this case, the additive inverse of a matrix A is simply the matrix with all elements negated.

2. **Multiplication Closure:**
    - For any square matrices A and B, the product AB is also a square matrix.

3. **Distributive Property:**
    - Multiplication distributes over addition. For any square matrices A, B, and C, the following properties hold:
        - A · (B + C) = (A · B) + (A · C)
        - (B + C) · A = (B · A) + (C · A)

So, square matrices of real numbers under addition and multiplication indeed satisfy these properties and are considered a ring.


### Fields

23. It isn’t possible to construct a trivial field using one element.

Why is that the case?

Hint: how many identity elements are needed? Hint: remember, the definition of a field is that the zero element is removed. An empty set cannot be a group.

> **Answer**

A field, by definition, requires two binary operators:

1. The first binary operator that makes the set an abelian (commutative) group.
2. The second binary operator, excluding the zero element, that also makes the set an abelian group.

The phrase "excluding the zero element" means that the second binary operator must form an abelian group for all elements in the set except for the zero element. In other words, every non-zero element in the set should have an inverse under the second binary operator, and this binary operation should be commutative for those non-zero elements.

Now, regarding the statement that there is no trivial field, here's why:

In a field, there must exist two binary operators, addition and multiplication, that satisfy specific properties. These properties include having an identity element and an inverse for each element, among others.

For addition, the identity element is typically denoted as 0, and for multiplication, the identity element is typically denoted as 1. In a field, 0 and 1 are distinct elements, and they play crucial roles as identity elements for addition and multiplication.

If we were to construct a trivial field using only one element, we would face a problem because we need two distinct identity elements: one for addition and one for multiplication. Since there is only one element in the set, it cannot simultaneously serve as both the identity element for addition (0) and the identity element for multiplication (1). Therefore, it is impossible to construct a trivial field with just one element.

In summary, a field requires two distinct identity elements (0 and 1) and specific properties for both addition and multiplication. A single-element set cannot fulfill these requirements, making it impossible to create a trivial field with only one element.


### Extra

24. Suppose you have a set `{monyet, kodok, burung, ular}`. Define a binary operator that turns it into a group using set-theoretic definitions.

> **Answer**

Let's define a binary operation '*' on the set {monyet, kodok, burung, ular} as follows:

monyet * kodok = burung
kodok * monyet = burung
burung * monyet = kodok
ular * any_element = ular (where 'any_element' can be monyet, kodok, burung, or ular)
Now, let's check if this binary operation forms a group:

Closure: This operation satisfies closure because the result of any two elements in the set is still an element in the set.

Associativity: The operation is defined such that it is associative. For example, (monyet * kodok) * burung = burung * burung = ular, and monyet * (kodok * burung) = monyet * burung = burung * burung = ular.

Identity Element: In this case, the identity element is "ular" because ular * monyet = ular * kodok = ular * burung = ular * ular = ular.

Inverse Element: For each element "a" in the set, we can find its inverse as follows:

monyet^(-1) = kodok because monyet * kodok = burung (identity element)
kodok^(-1) = monyet because kodok * monyet = burung (identity element)
burung^(-1) = burung because burung * burung = ular (identity element)
ular^(-1) = ular because ular * ular = ular (identity element)
Therefore, the binary operation '*' on the set {monyet, kodok, burung, ular} forms a group, as it satisfies all four group axioms.

```bash
cabal run extra_1
```


25. Find a binary operator that is closed but not associative for real numbers

> **Answer**

Consider a binary operator of **power** operator, denoted as "^". This operator takes two real numbers, a and b, and calculates a^b.

For example:

(2^3)^4 = 8^4 = 4096
2^(3^4) = 2^81 ≈ 2.41785 × 10^24
As you can see from the example, the results of these operations are not the same, demonstrating that the operator is not associative.

However, this operator is still closed, meaning that if you take any two real numbers a and b, a^b will be a real number as well.


26. Let our set be real numbers. Show a binary operator that is not closed

> **Answer**

Vonsider the binary operator of **division**, denoted as "/," on the set of real numbers. While division is a fundamental operation, it's important to note that it is not always closed on the set of real numbers, especially when considering division by zero.

In mathematical terms:

For any real numbers a and b, a / b is not defined if b = 0 because division by zero is undefined in mathematics.

This demonstrates that the binary operator of division is not closed on the set of real numbers when we consider the possibility of division by zero.


27. What algebraic structure is all odd integers under multiplication? All even integers under addition?

> **Answer**

The set of all odd integers under multiplication forms a group. 

Here's why:

1. Closure: When you multiply two odd integers, the result is also an odd integer. So, the set is closed under multiplication.

2. Associativity: Multiplication of integers is associative, so this property holds for the set of odd integers as well.

3. Identity Element: The identity element for multiplication is 1. In the set of odd integers, 1 is also an odd integer, so it serves as the identity element.

4. Inverse Element: For any odd integer 'a', its multiplicative inverse is 1/a, which is also an odd integer (assuming a ≠ 0). So, every element in the set has an inverse within the set.

On the other hand, the set of all even integers under addition forms an abelian (commutative) group. 

Here's why:

1. Closure: When you add two even integers, the result is also an even integer. So, the set is closed under addition.

2. Associativity: Addition of integers is associative, so this property holds for the set of even integers as well.

3. Identity Element: The identity element for addition is 0. In the set of even integers, 0 is also an even integer, so it serves as the identity element.

4. Inverse Element: For any even integer 'a', its additive inverse is -a, which is also an even integer. So, every element in the set has an inverse within the set.

5. Commutativity: Addition of integers is commutative, so this property holds for the set of even integers as well, making it an abelian group.


28. Let our group be 3 x 2 matrices of integers under addition. What is the identity and inverse? Can this be a cyclic group, why or why not? (Pay very close attention to the definition of cyclic group)

> **Answer**


In the case of 3x2 matrices of integers under addition, let's first discuss the identity element and inverses:

1. Identity Element: The identity element in this group would be a 3x2 matrix where all elements are zero. In matrix notation, this is represented as:

```sh
I = | 0 0 |
    | 0 0 |
    | 0 0 |
```

This matrix, when added to any other 3x2 matrix, will not change its value.

2. Inverse Elements: For any matrix A in this group, its additive inverse would be the matrix (-A). In other words, for every matrix A, there exists a matrix (-A) such that A + (-A) = I, where I is the identity matrix described above.

Now, let's discuss whether this group can be a cyclic group. A cyclic group is a group that can be generated by a single element, meaning every element in the group can be expressed as a power or combination of that single element.

In this case, the group of 3x2 matrices of integers under addition cannot be a cyclic group. The reason is that there is no single matrix that can generate all other matrices in the group through repeated addition.

In a cyclic group, you should be able to start with a single element (the generator) and add it to itself repeatedly to generate all other elements in the group. For example, in the additive group of integers under addition, the integer 1 generates the entire group because you can get any integer by adding 1 to itself repeatedly (e.g., 1, 2, 3, ...).

However, in the group of 3x2 matrices of integers, there is no single matrix that, when added to itself repeatedly, can generate all possible matrices in this group. The set of 3x2 matrices is more complex, and no single matrix can serve as a generator for the entire group.

Therefore, this group is not a cyclic group.


29. Demonstrate that

`n (mod p), n = ...-2,-1,0,1,2,...`

is a group under addition. Remember, you need to show that:

- the binary operator is closed
- the binary operator is associative
- an indentity exsits
- every element has an inverse
