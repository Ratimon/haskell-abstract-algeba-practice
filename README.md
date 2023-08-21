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

4. Using the formal definition of equality, show that if two finite sets have different cardinality, they cannot be equal. (Demonstrating this for infinite sets is a little trickier, so we skip that).

> **Answer**