# sexp-rewriter

A simple rewrite engine for s-expressions.  It reads rules from a file, and
rewrites a file containing a sequence of s-expression.  **It does not preserve
comments or whitespace.**

The aim for this program is to apply rewrite rules to SMT-LIB2 expressions such
as path conditions.

## Example

Suppose, you have the following input file (file names are given as comments at
the first line):

```
;; input.smt2
(assert
  (= (+ x x) (+ y y)))
(assert
  (= (* 1 x) 4))
```

And, you have the following rules:

```
;; arith.rules
(+ $1 $1) -> (* $1 2)
(* 1 $1) -> $1
```

Then, you can run this tool using Stack to rewrite the file:

```
stack run -- -r arith.rules -i input.smt2 -o output.smt2
```

And, it should produce the following in the output file (notice how whitespace
and comments are removed):

```
(assert (= (* x 2) (* y 2)))
(assert (= x 4))
```

## Full command-line options

## Syntax

The S-expression syntax is defined as follows:

```
s ∈ SExp ::= atom
           | '(' s* ')'
```

Where atoms are sequences containing letters, numbers, and non-parenthesis
non-semicolon symbols (the current parser accepts a pre-defined sequence of
characters). **Atoms cannot start with $**

Block comments are written as `(comment ...)`.  The support for block comments
is not great (they are handled in the lexer).

Line comments are written as `;...`.

### Rule syntax

The syntax for rules is as follows:

```
holedS ∈ SExpWithHole ::= atom
                        | hole
                        | '(' s* ')'
r ∈ Rule ::= holedS '->' holedS
hole ∈ Hole ::= '$' n
n ∈ ℤ
```

That is, a rule has two S-expressions with holes (the left-hand side and the
right-hand side of `->`), and each hole is of the form `$n` where `n` is an
integer.

## How the rules are applied

For each rule, we try to match the S-expression to the left-hand side, then
rewrite it to the right-hand side.  If it fails, we recursively try each
subtree.  This process is repeated until the rule no longer applies (i.e., a
fixpoint is reached).  Holes with the same number need to match the same
expressions.

### Example

Given the input `(= (+ x x) (+ y 1))`, suppose we are trying to apply `(+ $1 $1)
-> (* $1 2)`.

First, we try matching the whole expression `(= (+ x x) (+ y 1))` against `(+ $1
$1)` but that fails because `=` is not equal to `+`.

Then, we apply the rule recursively and collect the results:

- `=` does not match `(+ $1 $1)` because one side is an atom, the other is a
  list. We stop because this is a base case, and yield `=`.
- `(+ x x)` matches `(+ $1 $1)` and yields the substitution `$1 = x`.  We apply
  the substitution to the right-hand side of the rule, and yield `(* x 2)`.
- `(+ y 1)` does not match `(+ $1 $1)` because `$1` cannot be assigned to both
  `y` and `1`, so we recursively try each case and fail for the same reason as
  the `=` case, and yield `(+ y 1)`.

Finally, we collect the yielded values, and yield `(= (* x 2) (+ y 1))` as the
result.

Applying the rule again to this result does not change anything (returns the
same result), so we have reached a fixpoint.

### Caveats

- A rule fails if a variable appears only on the right-hand side.
- Some rules can cause infinite rules because there are no fixpoints. One
  example is `($1 $2) -> ($2 $1)`, applying this rule to `(x y)` will produce
  `(y x)` which will produce `(x y)` again and so on...

## Command-line options

The tool takes 3 options (all mandatory): **rule file**, **input file**, and
**output file**.  Run it with the option `--help` to see the specifics of how to
give these options.
