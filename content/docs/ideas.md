+++
title = "Ideas"
date = "2021-11-02"
categories = ["docs"]
+++

---

This page is an incomplete list of features that are currently
being considered for ante but for one reason or another are not
included in the language already. These features are listed
in no particular order.

---
# Polymorphic Variants or Union Types

Originally, polymorphic variants were devised as a scheme to infer
union types in a hindley-milner type system. Their inclusion into
ante would simplify error handling, allowing error unions to automatically
combine with each other:

```ante
// Using 'A for a polymorphic variant constructor
get (vec: Vec a) (index: usz) -> Result a 'NotFound =
    if index < len vec
    then Ok $ vec#index
    else 'NotFound

open (filename: string) -> Result File 'NoSuchFile =
    match File.open filename
    | Some file -> Ok file
    | None -> 'NoSuchFile

// Both error tags are automatically combined in the inferred
// return type: Result string [> 'NotFound | 'NoSuchFile]
foo (vec: Vec string) =
    filename = get? vec 0
    file = open? filename
    Ok (read_all file)
```

They are, however unable to accurately infer
types in all situations. Here are some problematic examples
along with their error messages (in OCaml 4.12.0) or inferred types
and what types we would ideally infer.

```ocaml
let foo x =
    match x with
    | `A -> x
    | `B -> `C

(*
Error: This expression has type [> `C ]
       but an expression was expected of type [< `A | `B ]
       The second variant type does not allow tag(s) `C
*)
```

Because we returned our input `x` which can only be either `A` or `B`,
polymorphic variants restrict our output types to be the same even though
we know in the branch `x` is returned in, it can only be `A`. When `C`
is returned from the second branch it then errors that it expected
(at most) `A` or `B` instead of inferring the preferred result of
```ocaml
foo : [< `A | `B ] -> [> `A | `C ]
```

Another problematic example is:

```ocaml
let bar = function
    | `A -> `B
    | y -> y
```
which infers
```ocaml
bar : ([> `A | `B ] as 'a) -> 'a
```
even though `A` can never be returned from the function, and the parameter
should be able to be any variant type instead of just `A` or `B`.
Ideally, it should infer something resembling
```ocaml
bar : [< `A | a] -> [> `B | a]
```

While useful, polymorphic variants aren't perfect. Generalized union types
may be more useful in they do not fall to these problems, but have the
caveat of requiring more advanced type machinary to infer them (namely
subtyping and intersection typing).
