# haskell-abstract-algeba-practice

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

> **Answer**

```bash
cabal run binary-operation_3
```

14. Work out for yourself that concatenating “foo”, “bar”, “baz” in that order is associative. Remember, associative means `(A op B) op C = A op (B op C)`.

> **Answer**

```bash
cabal run 
```

15. Give an example of `a magma` and `a semigroup`. The magma must not be a semigroup. Don’t use the examples above.

> **Answer**