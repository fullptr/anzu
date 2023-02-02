# anzu
An interpreted programming language written in C++. This started out as a stack-based language like Forth, it then took a route similar to Python with structures programming and duck-typing, and now I am drifting towards it being more simiar to C with a lot of "compile-time" checks including static typing.

## Features so far
* Fundamental types:
    1. Signed integral types `i32` and `i64`.
    1. Unsigned integral type `u64`.
    1. Floating point type `f64`.
    1. Boolean type `bool`.
    1. Character type `char`.
    1. Null type `null`.
    1. Typed pointers, eg: `&i64` is an `i64` pointer.

* Builtin fixed-size arrays:
    1. Declare elements up front: `l := [1, 2, 3]`.
    1. Declare repeat value and size: `l := [0; 5u]` (same as `l := [0, 0, 0, 0, 0]`).
    1. All objects in an array must be the same type.

* Variables:
    * Declare with `:=` operator: `x := 5`.
    * Assign to existing variable with `=` operator: `x = 6`.

* Comments using `#` symbol.

* `if` statements.

    ```rs
    if <condition> {
        ...
    } else if <condition> {
        ...
    } else {
        ...
    }
    ```

* basic `loop` loops (with `break` and `continue`):
    ```rs
    loop {
        ...
    }
    ```

* `while` loops:

    ```rs
    while <condition> {
        ...
    }
    ```

* `for` loops (for arrays only for now):

    ```rs
    for <name> in <array> {
        <body>
    }
    ```
    NOTE: `name` is a pointer to the element in the array

* `fn` function statements:

    ```rs
    fn factorial(i: u64) -> u64 {
        if (i == 0u) {
            return 1u; 
        }
        return i * factorial(i - 1u);
    }
    ```
* `struct` statements and member functions:
    ```cpp
    struct vec2
    {
        x: f64;
        y: f64;

        fn length2(self: &vec2) -> f64
        {
            return (self->x * self->x) + (self->y * self->y);
        }
    }
    ```
* All the common arithmetic, comparison and logical operators. More will be implemented.
* Builtin functions.

## The Pipeline
The way this langauage is processed and ran is similar to other langages. The lexer, parser, compiler and runtime modules are completely separate, and act as a pipeline by each one outputting a representation that the next one can understand. Below is a diagram showing how everything fits together.


```
Processing Pipeline

  Input
   |
Lexer    -- lexer.hpp     : Converts a .az file into a vector of tokens
   |
   |     -- token.hpp     : Definition of a token and utility
   |
Parser   -- parser.hpp    : Converts a vector of tokens into an AST
   |
   |     -- ast.hpp       : Definitions of AST nodes and utility
   |
Compiler -- compiler.hpp  : Converts an AST into a program
   |
   |     -- program.hpp   : Definitions of program op codes and utility
   |
Runtime  -- runtime.hpp   : Executes the program
   |
  Output

Common Modules
-- functions.hpp   : Definitions of builtin functions
-- object.hpp      : Definition of an object in anzu
-- type.hpp        : Definition of a type in anzu
-- vocabulary.hpp  : Definitions of keywords and symbols

Utility Modules (in src/utility)
-- overloaded.hpp  : A helper class to make std::visit simpler
-- peekstream.hpp  : A data structure used in the lexer
-- print.hpp       : Wrapper for std::format, similar to {fmt}
-- score_timer.hpp : An RAII class for timing a block of code
-- value_ptr.hpp   : A value-semantic smart pointer
-- views.hpp       : A collection of some helper views not in C++20
```

# Next Features (In ordoer of prio)
* Modules (in progress)
* Templates
* Function Pointers
* Filesystem Support
* Const
* Variants
