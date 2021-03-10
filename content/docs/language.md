+++
title = "Language Tour"
date = "2021-03-01"
categories = ["docs"]
+++

---

Ante is a low-level impure functional programming language. It is low-level
in the sense that types are not boxed by default and programmers can still
delve down to optimize allocation/representation of memory if desired. A
central goal of ante however, is to not force this upon users and provide
sane defaults where possible.
Compared to other low-level languages, ante is safe like rust but tries
to be easier in general, for example by avoiding the need for ownership
semantics through lifetime inference.

---
# Literals

## Integers

Integer literals can be of any signed integer type (i8, i16,
i32, i64, isz) or any unsigned integer type (u8, u16, u32, u64, usz) but by
default integer literals are [polymorphic](#int-trait). Integers come in
different sizes given by the number in their type that specifies
how many bits they take up. `isz` and `usz` are the signed and unsigned
integer types respectively of the same size as a pointer.

```ante
// Integer Literals are polymorphic, so if we don't specify their
// type via a suffix then we can use them with any other integer type.
100 + 1usz == 101

// Type error: operands of '+' must be of the same type
3i8 + 3u8

// Large numbers can use _ to separate digits
1_000_000
54_000_000_u64
```

## Floats

Floats in ante conform to the IEEE 754 standard for floating-point arithmetic
and come in two varieties: `f32` and `f64` for 32-bit floats and 64-bit
floats respectively. Floats have a similar syntax to integers, but with
a `.` separating the decimal digits.

```ante
// Float literals aren't polymorphic - they are of type f64
3.0 + 4.5 / 1.5

// 32-bit floats can be created with the f32 suffix
3.0f32
```

## Booleans

Ante also has boolean literals which are of the `bool` type and can be either
`true` or `false`.

## Characters

Characters in ante are a single, 32-bit [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
Note that since `string`s are UTF-8, multiple characters are packed into strings and if
the string contains only ASCII characters, it's size in memory is 1 byte per character in the string.

```ante
print 'H'
print 'i'
```

Character escapes can also be used to represent characters not on a traditional keyboard:

```ante
'\n' // newline
'\r' // carriage-return
'\t' // tab
'\0' // null character

'\u(xxxx)' // an arbitrary UTF-8 scalar value given by the
           // number 'xxxx' in hex
```

## Strings

Strings in ante are utf-8 by default and are represented via a pointer and
length pair. For efficient sub-string operations, strings are not null-terminated.
If desired, C-style null-terminated strings can be obtained by calling the `c_string` function.

```ante
print "Hello, World!"

// The string type is equivalent to the following struct:
type string =
    data: Ptr char
    len: usz

// C-interop often requires using the `c_string` function:
c_string (s: string) -> CString = ...

extern puts : CString -> i32
puts (c_string "Hello, C!")
```

## String Interpolation

Ante supports string interpolation via `${...}` within a string. Within
the brackets, arbitrary expressions will be converted to strings and spliced
at that position in the string as a whole.

```ante
name = "Ante"
print "Hello, ${name}!"
//=> Hello, Ante!

offset = 4
print "The ${offset}th number after 3 is ${3 + offset}"
//=> The 4th number after 3 is 7

```

---
# Variables

Variables are immutable by default and can be created via `=`.
Mutable variables are created via `= mut` and can be mutated
via the assignment operator `:=`. Also note that ante is strongly,
statically typed yet we do not need to specify the types of variables.
This is because ante has global [type inference](#type-inference).

```ante
n = 3 * 4
name = "Alice"

// We can optionally specify a variable's type with `:`
reading_about_variables: bool = true

// Mutable variables are created with `mut` and mutated with `:=`
pet_name = mut "Ember"
print pet_name  //=> Ember

pet_name := "Cinder"
print pet_name  //=> Cinder
```

## A brief note on mutability

Generally, mutability can make larger programs
more difficult to reason about, creating more bugs and increasing the cost
of development. However, there are algorithms that are simpler or more efficient
when written using mutability. Being a systems language, ante takes the position
that mutability should generally be avoided but is sometimes a necessary evil.

## Functions

Functions in ante are also defined via `=` and are just syntactic
sugar for assigning a lambda for a variable. That is, `foo1` and `foo2`
below are exactly equivalent except for their name.

```ante
foo1 a b =
    print (a + b)

foo2 = \a b ->
    print (a + b)
```

Since ante is impure, combining effects can trivially be done via sequencing
two expressions which can be done by separating the expressions with a newline.
`;` can also be used to sequence two expressions on the same line if needed. 

```ante
// We can specify parameter types via `:`
// and the function's return type via `->`
foo1 (a: u32) (b: u32) -> unit =
    print a
    print b
    print (a + b)
```

---
# Type Inference

Types almost never need to be manually specified due to the global type inference
algorithm which is based on an extended version of Hindley-Milner with let-polymorphism,
multi-parameter typeclasses (traits) and a limited form of functional dependencies.

This means ante can infer variable types, parameter types, function return types, and
even infer which traits are needed in generic function signatures.

```ante
// Something is iterable if we can call `next` on it and
// get either Some element and the rest of the iterator or
// None and we finish iterating
trait Iterable it -> elem with
    next: it -> Maybe (elem, it)

first_equals_two it =
    match next it with
    | Some (2, _) -> true
    | _ -> false
```
We never gave any type for `first_equals_two` yet ante infers its type for us as
`a -> bool given Iterable a i32` - that is a function that returns a `bool` and takes
a generic parameter of type `a` which must be an iterator producing elements of type `i32`.

---
# Significant Whitespace

Ante uses significant whitespace to help declutter source code and prevent bugs
(such as Apple's infamous
[goto fail](https://nakedsecurity.sophos.com/2014/02/24/anatomy-of-a-goto-fail-apples-ssl-bug-explained-plus-an-unofficial-patch/)
bug). In general, ante tries to be simple with its whitespace semantics:
if the next line is indented 2+ or more spaces from the previous non-commented line
then an indent token is issued. If the lines differ by only 1 space then it is
considered to be a mistake and an error is issued. There is no notion of indenting
to or past an exact column like in Haskell's [offside rule](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3).

Secondly, anytime an unindent to a previous column occurs, an unindent token is issued.
Indents and unindents follow a stack discipline, each unindent is a return to a previous
indentation level rather than to a new one. So the following program is invalid since the
`else` was not unindented to the previous indent level.

```ante
if true then
        print "foo"
    else
        print "bar"
```

Thirdly, when a newline between two lines of code at the same indent level occurs,
a newline token is issued to separate the two expressions.

From these three rules we get `Indent`, `Unindent`, and `Newline` tokens which the
parser can parse just as if they were `{`, `}`, and `;` tokens in the source program.

## Line Continuations

With the above 3 rules the syntax is transformed into one with the equivalent of
explicit `{`, `}`, and `;` tokens. ~95% of programs just work now and ante could stop
there if it wanted to, and for a long time it did. A problem arises however with the
third rule of using newlines as `;` tokens. Sometimes, users may wish to continue an
expression onto multiple lines. This is a problem with parallels of automatic semicolon
insertion in other languages. The main difference being ante also has significant whitespace
to help it clue into this problem.

Ante's original solution was more of a band-aid. It followed the python example of continuing
lines with `\` at the end of a line which would tell the lexer not to issue a newline token.
There was also a similar rule for elliding newlines while we were inside `()` or `[]` pairs.
This solution was quite annoying in practice however. Ante is much more expression-oriented
than python and particularly when working the [pipeline operators](#pipeline-operators) we would end up with a long
chain of lines ending with `\`:

```ante
data  \
|> map (_ + 2) \
|> filter (_ > 5) \
|> max!

a = 3 + 2 *  \
    5 + 4    \
    * data

what_a_long_function_name \
    function_arg_with_long_name1 \
    function_arg_with_long_name2 \
    (a + 1)
```

In practice this ugly bit of syntax tended to discourage the otherwise good practice of
splitting long lines onto multiple lines. Ante thus needed a better solution. The goals
of the new solution were to be unambiguous, ergnomic, match a developer's mental model
of their program, and to issue error messages if needed instead of silently inferring the wrong thing.

This was initially difficult to solve but eventually ante landed on a solution based upon the
observance that when programmers want to continue lines, they almost always use indentation
to do so. Thus, to continue an expression in ante, the continuation must just be indented and
you can continue to use that same indentation level if you need multiple lines.

This is done by tracking when an indent is expected in the lexer and
only issuing the indent (and following unindent) if so. Ante's grammar is designed in
such a way that the lexer only needs to look at the previous token to find out if it expects
an indent afterward or not. These tokens that may have indentation after them are `if`, `then`,
`else`, `while`, `for`, `do`, `match`, `with`, along with `=`, `->`, and the assignment operators.
Semantically, these are the tokens needed for if-expressions, loops, match expressions, definitions,
and assignments. This is the list of tokens the programmer would normally need an indent for a block
of code after - it is analogous to knowing when you need to type `{` in curly-braced languages.

Note that an important part of this being implemented entirely in the lexer is that operator precedence
after continued lines just works (it is harder than it may seem if continuation is a parser rule).

When the lexer sees an indent that without one of these tokens preceeding it, it does not issue
an indent token and also does not issue newline tokens for any expression at that same level of
ignored indentation. Note that this is tracked on a per-block basis, so if we wanted we could
also still use constructs like `if` inside these blocks with ignored indentation - since we'd
be indenting to a new level and that new level would have the indent tokens issued as normal.

With this rule, we can continue any line just by indenting it. Here's the previous example again
with the new rule:

```ante
// |> is actually the exception to the "programmers typically indent
// continuation lines" rule. Naively trying the following lines however
// shows us another nice property of the rules above: we get an error
// if we mess up.
data
|> map (_ + 2)  // error here, |> has no lhs! We must continue the line by indenting it
|> filter (_ > 5)
|> max!

// Here's the fixed, indented version
map data (_ + 2)
    |> filter (_ > 5)
    |> max!

// The other examples work as expected
a = 3 + 2 *
    5 + 4
    * data

what_a_long_function_name
    function_arg_with_long_name1
    function_arg_with_long_name2
    (a + 1)
```

---
# Operators

Operators in ante are essentially infix functions, they're even
defined in `trait`s the same way many functions are. Here are some
standard one's you're probably used to:

```ante
// The standard set of numeric ops with operator precedence as you'd expect
trait Add a with
    (+): a a -> a

trait Sub a with
    (-): a a -> a

trait Mul a with
    (*): a a -> a

trait Div a with
    (/): a a -> a

// % is modulus, not remainder. So -3 % 5 == 2
trait Mod a with
    (%): a a -> a
```

```ante
// Comparison operators are implemented in terms of the `Cmp` trait
trait Cmp a with
    compare: a a -> Ordering

type Ordering = | Lesser | Equal | Greater

(<) a b = compare a b == Lesser
(>) a b = compare a b == Greater
(<=) a b = compare a b != Greater
(>=) a b = compare a b != Lesser
```

There are also various compound assignment operators for convenience
when mutating data (`+=`, `-=`, and friends).

Logical operators have their names spelled out fully:
```ante
if true and false then print "foo"

if false or true then print "bar"

if not false then print "baz"
```

`and` binds tighter than `or`, so the following prints `true`:
```ante
if false and true or true and true then
    print true

// parsed as:
if (false and true) or (true and true) then
    print true
```

Since fiddling with individual bits is not a common operation, there
are no bitwise operators. Instead there are functions in the
`Bits` module for dealing with bits. If you wish these functions could
be infix you are in luck - any function taking two arguments can be
infix using [infix function](#infix-functions) syntax.

## Subscript Operator

The subscript operator for retrieving elements out of a collection
is spelled `a#i` in ante. The more common `a[i]` syntax would be
ambiguous with a function call to a function `a` taking a single
argument that is the array literal `[i]`.

`#` has a higher precedence than all other binary operators. The following
example shows how to average the first and second elements in an array for
example:

```ante
average_first_two array =
    (array#0 + array#1) / 2
```

## Dereference Operator

Dereferencing pointers in ante can be done via the `@` operator which
gives you the value "at" that location. Ante uses this over the traditional
unary `*` to avoid overloading `*` with too many meanings.

Note that there is no C/C++-like `->` operator for dereferencing a struct
to get its field. `struct.field` in ante will work as expected regardless
of if `struct` is indeed a struct or is a pointer to a struct.

Raw-pointer dereferencing is not done often in ante so it is rare you
will see this operator in practice. One place you may see it is with C interop:

```ante
buffer = mut malloc (Mem.sizeof string)
@buffer := "foo"
free buffer
```

## Infix Functions

Any function taking two arguments can be used as an infix function by surrounding
its name with grave characters: ``` `` ```. Infix functions are left-associative
and have a high precedence just below `#`.

```ante
add a b = a + b
mul = (*)

print (3 `add` 2 `mul` 4)
//=> print ((3 + 2) * 4)

// Infix function syntax is commonly used with the Bits module
import Bits

1 `shiftl` 2  //=> 4
5 `xor` 3     //=> 1

hash += hash `shiftl` 5 + key
// equivalent to:
// hash := hash + ((shiftl hash 5) + key)
```

## Pipeline Operators

The pipeline operators `|>` and `<|` are sugar for function application and
serve to pipe the results from one function to the input of another.

`x |> f y` is equivalent to `f x y`. It is left-associative so `x |> f y |> g z`
desugars to `g (f x y) z`. This operator is particularly useful for chaining
iterator functions:

```ante
// Parse a csv's data into a matrix of integers
parse_csv (text: string) -> Vec (Vec i32) =
    lines text
        |> skip 1  // Skip the column labels line
        |> split ","
        |> map parse!
        |> collect
```

In contrast to `|>`, `<|` is right associative and applies a function on its
left to an argument on its right. It is spelled `$` in haskell. Where `|>`
is used mostly to spread operations across multiple lines, `<|` is often
used for getting rid of parenthesis on one line.

```ante
print (sqrt (3 + 1))

// Could also be written as:
print <| sqrt <| 3 + 1
```

## Pair Operator

Ante does not have tuples, instead it provides a right-associative pair
operator `,` to construct a value of the pair type. We can use it like
`1, 2, 3` to construct a value of type `i32, i32, i32`
which in turn is just sugar for `Pair i32 (Pair i32 i32)`.

Compared to tuples, pairs are:

1. Simpler: They do not need to be built into the compiler or its type
system. Instead, they can be defined as a normal struct type in the standard
library:

```ante
type Pair a b = first: a, second: b
```

2. Easier to work with: Because pairs are just normal data types, we get
all the capabilities of normal types for free. For example, we know all pairs
will have exactly two fields. This makes creating `impl`s for them much easier.
Lets compare the task of converting a tuple to a string with doing the same for pairs.
With tuples we must [create a different impl for every possible tuple size](https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Show.html#line-268).
with pairs on the other hand the simple implementation works for all sizes:

```ante
impl Cast (a, b) string given Cast a string, Cast b string with
        cast (a, b) = "${a}, ${b}"
```

3. Just as efficient: both pairs and tuples have roughly the same representation
in memory (the exact same if you discount allignment differences).

4. More composable: having the right-associative `,` operator means we can
easily combine pairs or add an element if needed. For example, we can
implement `zip3` for nested pairs of length 3 in terms of `zip`:

```ante
// given we have zip : (List a) (List b) -> List (a, b)
zip3 (a: List a) (b: List b) (c: List c) -> List (a, b, c) =
        zip a (zip b c)
```

    - Another place this shows up in is when deconstructing pair values.
    Lets say we wanted to define a function `first` for getting the first
    element of any tuple of length >= 2 (remember, we are using nested pairs,
    so there are no 1-tuples!), and `third` for getting the third
    element of any tuple of length >= 3. We can define the functions:

    ```ante
    first (a, _) = a
    third (_, _, c) = c

    first (1, 2) == 1
    first ("one", 2.0, 3, 4) == "one"

    third (1, 2, 3) == 3
    third (1, "two", 3.0, "4", 5.5) == (3.0, "4", 5.5)

    // If the above is confusing, remember that , is right-associative,
    // so the parser will parse `third` and the call as follows:
    //
    // third (_, (_, c)) = c
    // third (1, ("two", (3.0, ("4", 5.5)))) == (3.0, ("4", 5.5))
    ```

        Note that to work with nested pairs of any length >= 3 instead of >= 4, our implementation of
        `third` will really return a nested pair of `(third, rest...)` for
        pairs of length > 3. This is usually what we want when working with
        generic code (since it also works with nested pairs of exactly length 3 and
        enables the nice syntax in the next section).

One last minor advantage of pairs is that we can use the fact that `,` is
right-associative to avoid some extra parenthesis compared to if we had tuples.
A common example is when enumerating a tuple, most languages would need two sets
of parenthesis but in ante since tuples are just nested pairs you can just add another `,`:

```ante
pairs = [(1, 2), (3, 4)]

// Other languages require deconstructing with nested parenthesis:
for (i, (one, two)) in enumerate pairs do
    print "Iteration ${i}: sum = ${one + two}"

// But since `,` is just a normal operator,
// the following version is equally valid
for (i, one, two) in enumerate pairs do
    print "Iteration ${i}: sum = ${one + two}"
```

Finally, its necessary to mention that the earlier `Cast` example printed nested
pairs as `1, 2, 3` where as the `Show` imple in haskell printed tuples as `(1, 2, 3)`.
If we wanted to surround our nested pairs with parenthesis we have to work a bit
harder by having a helper trait so we can specialize the impl for pairs:

```ante
impl Cast (a, b) string given Cast a string, Cast b string with
    cast pair = "(${to_string_helper pair})"

trait ToStringHelper t with
    to_string_helper (x: t) -> string = cast x

// Specialize the impl for pairs so we can recurse on the rhs
impl ToStringHelper (a, b) with
    to_string_helper (a, b) = "${a}, ${to_string_helper b}"
```

And these two functions will cover all possible lengths of nested pairs.

## try and ?

Since ante does not have exceptions, the natural approach for error handling
is using the `Maybe` and `Result` types. To avoid the boilerplate of manually
matching on these types to propagate up errors, ante provides the `?` operator.
By default, an expression `foo ?` is equivalent to:

```ante
match cast foo : Result t e with
| Ok x -> x
| Err e -> return error e
```

which uses the `try` trait:

```ante
trait Try t -> ok err given Cast t (Result ok err) with
    error: err -> t
```

Here's an example function that handles some optional values,
written with and without the `?` operator:

```ante
add_even_numbers1 (a: string) (b: string) -> Maybe u64 =
    n1 = match parse a with
        | Ok n -> n
        | Err e -> return None

    n2 = match parse b with
        | Ok n -> n
        | Err e -> return None

    if n1 % 2 == 0 and n2 % 2 == 0 then
        Some (n1 + n2)
    else
        None

add_even_numbers2 (a: string) (b: string) -> Maybe u64 =
    n1 = parse a ?
    n2 = parse b ?

    if n1 % 2 == 0 and n2 % 2 == 0 then
        Some (n1 + n2)
    else
        None
```

By default `?` will return early in the current function. Sometimes,
we may want to only "return" to an intermediate point where we can
better handle the error. This is where `try` comes in - any `?`s
on its right hand side will "return" to the innermost try expression
rather than the function as a whole. Here's an example:

```ante
add_optionals_or_default (a: Maybe i32) (b: Maybe i32) (default: i32) -> i32 =
    result = try a? + b?
    result.unwrap_or default
```

## ! Operator

In addition to `?`, ante has another error-handling operator `!`. Where
`?` forwards up an error, `!` unwraps the error, asserting at runtime that
the error is impossible and panicing if it occurs. Unlike the
`unwrap` function however - `!` operates on another function as its argument.
It takes a function on its lhs that returns a value that implements `Try` and
returns a function taking the same arguments but returning only the non-error
value. It is similar to the function in pseudocode below:

```ante
given Try t ok err
(!) (f: Args -> t) -> (Args -> ok) = \args ->
    match cast (f args) with
    | Ok val -> val
    | Err e -> panic "Tried to unwrap error value ${e}"
```

While many operations can conceptually fail, in practice `unwrap` tends to
be used a fair amount since there are still situations we do not expect to
fail. For these situations `!` is quite useful since it still functions as
a visual indication something can fail but also obscures our business logic
less than `unwrap`s do. Here's an example:

```ante
find_least_cost_neighbor graph =
    get_root graph
    |> unwrap
    |> get_neighbors
    |> min_by \node. unwrap (node_cost node)
    |> unwrap

// Compared to:
find_least_cost_neighbor graph =
    get_root! graph
    |> get_neighbors
    |> min_by! node_cost!
```

Note that while `!` can conceptually be used for all errors, in practice
it is not a good idea to do so. If you do not know or want to assert an
error is impossible, then it is a better idea to propagate up the error
via `?` to a callsite that knows more. Resultingly, library code rarely
uses `!` and application code tends to use it more often,
but usually still less than `?`.

---
# Lambdas

Lambdas in ante have a syntax familiar to those used to haskell:
`\arg1 arg2 ... argN -> body`. Additionally a function definition
`foo a b c = body` is really just sugar for a variable assigned to
a lambda: `foo = \a b c -> body`. Lambdas can also capture part of
the variables in the scope they were declared. When they do this,
they are called closures:

```ante
augend = 2
data = 1..100

map data \x. x + augend
//=> 3, 4, 5, ..., 100, 101
```

## Explicit Currying

While ante opts out of including implicit currying in favor of better
error messages, it does include an explicit version where arguments
of a function can be explicitly curried by placing `_` where that argument
would normally go. For example, in the following example, `f1` and `f2` are
equivalent:

```ante
f1 = \x. x + 2
f2 = _ + 2
```

Compared to implicit currying, explicit currying lets us curry function
arguments in whatever order we want:

```ante
add3 a b c = a + b + c

g1 = add3 _ 0 _

// g1 is equivalent to:
g2 = \a c -> add3 a 0 c
```

Explicit currying only curries the innermost function, so using
it with nested function calls will yield a type error unless the
outermost function is expecting another function:

```ante
// Nesting _ like this gives a type error:
// add3 expects an integer argument but a function was given.
nested = add3 1 2 (_ + 3)

// To make nested a function, it needs to be rewritten as a lambda:
nested = \x -> add3 1 2 (x + 3)

// Or a function definition
nested x = add3 1 2 (x + 3)
```

`_` really shines when using higher order functions and iterators:
```ante
// Given a matrix of Vec (Vec i32), output a string formatted like a csv file
map matrix to_string
|> map (join _ ",") // join columns with commas
|> join "\n"        // and join rows with newlines.
```

---
# Control-Flow

Ante's control flow keywords should be very familiar to any programmer used
to expression-based languages.

If expressions expect a boolean condition (there are no falsey values) and
conditionally evaluate and return the then branch if it is true, and the
else branch otherwise. The else branch may also be omitted - in that case
the whole expression returns the unit value. The if condition, then branch,
or else branch can either be single expressions or an indented block expression.

```ante
three = if false then 2 else 3

if should_print () then
    print three
```

Ante also has 2 kinds of loops: while loops and for loops. While loops
run until their condition is false:

```ante
while input "continue (y/N)?" == "y" do
    print "Looping!"
```

Ante does not have do-while loops but they can be emulated by putting
the computation in the condition block. Since the condition must still
be at the end of this block we end up with an equivalent loop that runs
at least once:

```ante
while
    command = input "> "
    execute command
    command != "exit"
do ()
```

For loops in ante loop over anything that is `Iterable`. The basic for loop:

```ante
for x in xs do
    ...
```

Uses the following `Iterable` and `Iterator` traits:

```ante
trait Iterable t -> it e given Iterator it e with
    into_iterator: t -> it

trait Iterator it -> e with
    // Optionally return the next element and rest of the iterator.
    // If this returns None, the loop is done
    next: it -> Maybe (e, it)
```

And desugars into an equivalent of the following while loop:

```ante
iterator = mut into_iterator xs
while true do
    match next iterator with
    | None -> break
    | Some (x, rest) ->
        iterator := rest
        ...
```

As seen above, you can also use `break` (and `continue`) to break out of
a loop early or to continue to the next iteration. Both take an optional
integer argument that represents the number of loops to break out of.
Omitting this defaults it to `1`.

```ante
for x in 0..1_000_000 do
    for y in 0..1_000_000 do
        if x * y == 123456 then
            print "found pair ${x} and ${y}"
            break 2
```

---
# Pattern Matching

Pattern matching on algebraic data types can be done with a `match`
expression:

```ante
match foo with
| Some bar -> print bar
| None -> ()
```

Since `match` is an expression, each branch must match type. The value
of the matched branch is evaluated and becomes the result of the whole
match expression. The compiler will also warn us if we forget a case
or include one that is redundant and will never be matched.

```ante
// Error: Missing case: Some None
match foo with
| Some (Some bar) -> ...
| None -> ...
```

In addition to the usual suspects (tagged-unions, structs, pairs), we can
also include literals and guards in our patterns and it will work as
we expect:

```ante
type IntOrString =
   | Int i32
   | String string

match Int 7 with
| Int 3 -> print "Found 3!"
| Int n if n < 10 -> print "Found a small Int!"
| String "hello" -> print "Found a greeting!"
| value -> print "Found something else: ${value}"
```

Note that there are a few subtle design decisions:

1. All type constructors must be capitalized in ante, so when
   we see a lower-case variable in a pattern match we know we
   will always create a new variable rather than match on some
   nullary constructor (like `None`).

2. Each pattern is prefixed with `|` rather than being indented like
   in some other languages. Doing it this way means if we indent the
   body as well, we only need to indent once past the `match` instead
   of twice which saves us valuable horizontal space.

---
# Types

Ante is a strongly, statically typed language with global type inference.
Types are used to restrict the set of values as best as possible such that
only valid values are representable. For example, since references in ante
cannot be null we can instead represent possibly null references with
`Maybe (ref t)` which makes it explicit whether a function can accept or
return possibly null values.

## Type Definitions

Both struct and tagged union types can be defined with the
`type Name args = ...`construct where `args` is the space-separated
list of type variables the type is generic over.

You can define struct types with commas separating each field or
newlines if the type spans multiple lines:

```ante
type Person = name: string, age: u8

type Vec a =
    data: Ptr [a]
    len: usz
    capacity: usz
```

Tagged unions can be defined with `|`s separating each variant.
The `|` before the first variant is mandatory. Ante currently has
no support for untagged C unions.

```ante
type Maybe t =
   | Some t
   | None

type Result t e =
   | Ok t
   | Err e
```

## Type Annotations

Even with global type inference, there are still situations where
types need to be manually specified. For these cases, the `x : t`
type annotation syntax can be used. This is valid anywhere an expression
or irrefutable pattern is expected. It is often used in practice
for annotatiing parameter types and for deciding an unbounded generic
type - for example when parsing a value from a string then printing it.
Both operations are generic so we'll need to specify what type we should
parse out of the string:

```ante
parse_and_print_int (s: string) -> unit =
    x = parse s : i32
    // alternatively we could do
    // x: i32 = parse s
    print x
```

## Refinement Types

Refinement types are an additional boolean constraint on a normal type.
For example, we may have an integer type that must be greater than 5.
This is written like `x : i32 given x > 5`. These refinements can be
written into the given clauses of functions, and are mostly restricted
to numbers or "uninterpreted functions." This limitation is so we can
infer these refinements like normal types. If we instead allow any value
to be used in refinements we would get fully-dependent types for which
inference and basic type checking (without manual proofs) is undecidable.

Refinement types can be used to ensure indexing into an array is always valid:

```ante
given index < len a
get (a: Array t) (index: usz) -> t = ...

a = [1, 2, 3]
get a 2  // valid
get a 3  // error: couldn't satisfy 3 < len a

n = random_in (1..10)
get a n  // error: couldn't satisfy n < len a

// The solver is smart enough to know len a > n <=> n < len a
if len a > n then
    get a n  // valid
```

You can also use uninterpreted functions to tag values. The following
example uses this technique to tag arrays returned by the `sort`
function as being sorted, then restricting the input of `binary_search`
to only sorted arrays:

```ante
// You can name a return type to
// use it in a 'given' expression
given sorted ret
sort (array: Array t) -> ret: Array t = ...

given sorted array,
      index < len array
binary_search (array: Array t) (elem: t) -> Maybe (index: usz) = ...
```

In contrast to contracts in other languages, these refinements are in
the type system and are thus all checked during compile-time with
the help of a SMT solver.

---
# Traits

While unrestricted generic functions are useful, often we don't want
to abstract over "forall t." but rather abstract over all types that
have certain operations available on them - like adding. In ante, this
is done via traits. You can define a trait as follows:

```ante
trait ToString t with
    to_string: t -> string
```

Here we say `to_string` is a function that take in a `t` and returns a `string`.
With this, we can write another function that can abstract over all `t`'s that
can be converted to strings:

```ante
given ToString t
print_to_string (x: t) -> unit =
    print (to_string x)
```

Just like types, we can leave out all our traits in the `given` clauses
and they can still be inferred.

Traits can also define relations over multiple types. For example,
we may want to be more general than the `ToString` cast above -
that is we may want to have a trait that can cast to any result
type. To do this we can have a trait that constrains two generic types
such that there must be a cast function that can cast from the
first to the second:

```ante
trait Cast a b with
    cast: a -> b

// We can cast to a string using
cast 3 : string
```

## Impls

To use functions like `to_string` or `print_to_string` above,
we'll have to `impl`ement the trait for the types we want
to use it with. This can be done with `impl` blocks:

```ante
impl ToString bool with
    to_string b =
        if b then "true"
        else "false"
```

Then, when we call a function like `print_to_string` which
requires `ToString t` we can pass in a `bool` and the
compiler will automatically find the `ToString bool` impl
in scope and use that:

```ante
print_to_string true  //=> outputs true
```

## Functional Dependencies

Some languages have a concept called [associated types](https://doc.rust-lang.org/1.16.0/book/associated-types.html).
These are often necessary to define some traits properly which
have multiple type parameters but in which we want the type
of some parameters to depend on others. Ante offers a limited
form of functional dependencies for this which is equivalent to
the associated types approach but with a nicer notation.

To illustrate the need for such a construct, lets say we wanted
to abstract over an array's `get` function:

```ante
get (array: Array t) (index: usz) -> Maybe t = ...
```

We want to be able to use this with any container type, but how?
We can start out with a trait similar to our `Cast` trait from
before:

```ante
trait Container c elem with
    get: c usz -> Maybe elem
```

At first glance this looks fine, but there's a problem: we
can implement it with any combination of `c` and `elem`:

```ante
impl Container (Array i32) i32 with
    get a = ...

impl Container (Array i32) string with
    get a = None
```

But we already had an impl for `Array i32`, and defining a
way to get another element type from it makes no sense!
This is what associated types or ante's restricted functional
dependencies solve. We can modify our Container trait to
specify that for any given type `c`, there's only 1 valid
`elem` value. We can do that by adding an `->`:

```ante
trait Container c -> elem with
    get: c usz -> Maybe elem

impl Container (Array a) a with
    get a = ...

// Error! There there is already an impl for
// `Container (Array a) a` in scope!
impl Container (Array a) string with
    get a = ...
```

As a bonus, this information is also used during type inference
so if we have e.g. `e = get (b: ByteString) 0` and there
is a `impl Container ByteString u8` in scope then we also
know that `e : u8`.

Note that using a functional dependency in a trait signature
looks a lot like separating the return type from the arguments
of a function (both use `->`). This was intentional; a good
rule of thumb on when to use functional dependencies is if
the type in question only appears as a return type for the
function defined by the trait. For the `Container` example
above, `elem` is indeed only used in the return type of `get`.
The most notable exception to this rule is the `Cast` trait
defined earlier in which it is useful to have two impls
`Cast i32 string` and `Cast i32 f64` to cast integers
to strings and to floats respectively.

## Coherence

Ante currently has no concept of global coherence for impls,
so it is perfectly valid to define overlapping impls or define
impls for types outside of the modules the type or trait were
declared in. If there are ever conflicts with multiple valid
impls being found, an error is given at the callsite. This
lack of restriction may change in the future.

## Int Trait

Ante has quite a few [integer types](#integers) so one question
that gets raised is what is the type of an integer literal?
If we randomly choose a type like `i32` then when using all
other integer types we'd have to constaintly annotate our
operations with the type used which can be annoying. Imagine
`a + 1u64` every few lines.

Instead, integer literals are polymorphic over the `Int` trait:

```ante
trait Int a with
    // no operations, this trait is built into
    // the compiler and is used somewhat like a typetag
```

When we use an integer with no specific type, the integer literal
keeps its generic type. This sometimes pops up in function signatures:

```ante
// This works with any integer type
given Int a
add1 (x: a) -> a =
    x + 1
```

If we do use it with a specific type however, then just like with
normal generics, the generic type variable is constrained to be
that concrete type (and the concrete type must satisfy the `Int`
constraint - ie it must be a primitive integer or we get a compile-time error).

```ante
// Fine, we're still generic over a
given Int a
foo () -> a = 0

x: i32 = 1  // also fine, we constrained 1 : i32 now

y = 2u16  // still fine, now we're specifying the type
          // of the integer literal directly
```

## Member Access Traits

If we have multiple types with the same field in scope:

```ante
type A = foo: i32

type B = foo: string
```

Then we are left with the problem of deciding what the type
of the `x.foo` expression should be:

```ante
// Does this work?
// - If so what type do we get?
// - If not, what is the error?
get_foo x = x.foo
```

Ante solves this with member access traits. Whenever ante
sees an expression like `x.foo` it makes a new trait:

```ante
trait .foo struct -> field with
    (.foo) : struct -> field
```

---
# Modules

Ante's module system files a simple, hierarchical structure
based on the file system. Given the following file system:

```
.
├── foo.an
├── bar.an
├─┬ baz
│ ╰── nested.an
╰─┬ qux
  ├── nested.an
  ╰── qux.an
```

We get the corresponding module hierarchy:

```ante
Foo
Bar
Baz.Nested
Qux
Qux.Nested
```

Note how `qux/qux.an` is considered a top-level module
because it matched the name of the folder it was in and
how `baz/nested.an` is under `Baz`'s namespace because it was
in the `baz` folder. The two `nested.an` files are also in
separate parent modules so there is no name conflict.

## Imports

Importing all symbols within a module into scope can be
done with an `import` expression. Lets say
we were using the module hierarchy given in the
[section above](#modules). In our `Baz.Nested` file we
have:

```ante
nested_baz = 0

print_baz () =
    print "baz"

get_baz () = "baz"
```

To use these definitions from `Foo` we can import them:

```ante
import Baz.Nested

baz = get_baz ()
print "baz: ${baz}, nested_baz = ${nested_baz}"
```

We can also import only some symbols into scope:

```ante
import Baz.Nested.(print_baz, get_baz)

print (get_baz ())
print_baz ()
```

You'll note that `import`s are not qualified by default,
this may change in the future.

---
# Lifetime Inference

To protect against common mistakes in manual memory management
like double-frees, memory leaks, and use-after-free, ante automatically
infers the lifetime of `ref`s. If you're familiar with rust's
lifetime system, this works in a similar way but is intentionaly
less restrictive since it abandons the ownership rule of only
allowing either a single mutable reference or multiple immutable ones.
Also unlike rust, ante hides the lifetime parameter on references.
Since it is inferred automatically by the compiler there is no need
to manually mess around with them. There is a tradeoff compared to
rust however: to accomplish this hands-off approach ante typically
infers `ref`s to live longer than they need to.

`ref`s can be created with `new : a -> ref a` and the underlying
value can be accessed with `(@) : ref a -> a`. Here's a simple
example 

```ante
get_value () -> ref i32 =
    three = 3
    // This & operation will copy and allocate 3
    &three

value = get_value ()

// the ref value is still valid here and
// is deallocated when it goes out of scope.
print value
```

The above program is compiled to the equivalent of destination-passing
style in C:

```c
void get_value(int* three) {
    *three = 3;
}

int main() {
    int value;
    get_value(&value);
    print(value); // print impl is omitted for brevity
}
```

The above program showcased we can return a `ref` value to extend its
lifetime. Perhaps a more standard operation is to just use them as
temporary references:

```ante
type VeryLarge = ...

some_operation (x: ref VeryLarge) (n: i32) -> Result i32 string
    ... // do things with x
    Ok n

verylarge: VeryLarge = ...

some_operation &verylarge 2
```

Unlike C++-references the lifetime inference system will ensure
this reference never outlives the value (since the lifetime will
automatically be lengthened), and compared to Rust, you will never
get an error that the lifetime was too short since it is always
inferred to be long enough.

## Details

Internally, lifetime inference of refs uses the original Tofte-Taplin
stack-based algorithm. This algorithm can infer references which
can be optimized to allocate on the stack instead of the heap
even if it needs to be allocated on a prior stack frame. The
tradeoff for this is that, as previously mentioned, the inferred
lifetimes tend to be imprecise. As such, `ref`s in ante are meant
to be used for temporary unowned references like where you'd use
`&` in rust. It is not a complete replacement for smart pointer types
such as `Box` and `Rc` (unless you're fine with using more memory).
The place where `ref`s are typically worst are in implementing container types.
`ref`s are implemented using memory pools on the stack under the
hood so any container that wants to free early or reallocate and
free/resize memory (ie. the vast majority of containers) should use
one of the smart pointer types to hold their elements instead.

---
# Extern

Ante's C FFI is currently limited to `extern` functions.
Without `extern`, all definitions must be initialized
with a value and any names used may be mangled in the
compiled output.

You can use extern by declaring a value and giving it
a type. Make sure the type is accurate as the compiler
cannot check these signatures for correctness:

```ante
extern puts: C.String -> C.Int
```

You can also use extern with a block of declarations:

```ante
extern
    exit: C.Int -> never_returns
    malloc: usz -> Ptr a
    printf: C.String ... -> C.Int
```

Note that you can also use varargs (`...`) in these declarations
and it will work as expected. There is currently no equivalent
to an untagged C union in ante so using any FFI that requires
passing in unions will require putting them behind pointers
in ante.
