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

* Pointers:
    1. Uses trailing syntax for both taking addresses and dereferencing.
    1. eg: `i64&` is an `i64` pointer.
    1. If `ptr` is an `i64&`, then `ptr@` is the int that it points to.
    1. Uses `@` instead of the familiar `*` because using `*` in trailing syntax can be ambigious with multiplication. Plus I like it more; it signals that I'm using the value "at" the pointer.

* Arrays:
    1. Fixed size arrays with statically known size.
    1. Declare elements up front: `l := [1, 2, 3]`.
    1. Declare repeat value and size: `l := [0; 5u]` (same as `l := [0, 0, 0, 0, 0]`).
    1. All objects in an array must be the same type.

* Spans:
    1. Non-owning views over arrays, made up of a pointer + a size.
    1. Create a span from an array using trailing `[]`.
    1. eg: If `l` is an array of 5 `i64`s, then `l[]` is an `i64[]`.
    1. Slicing syntax `l[0 : 2]` for creating subspans.
    1. Arrays can automatically convert to spans when passing to functions.

* Function Pointers:
    1. Function names resolve to function pointers which can be passed to functions.
    1. Syntax for function pointer types: `fn(<arg_types>) -> <return_type>`.

* Variables:
    * Declare with `:=` operator and either `let` or `var`: `let x := 5` or `var x := 5`.
    * `let` declares a const value and `var` declares a mutable value.
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

        fn length2(self: (const vec2)&) -> f64
        {
            return (self.x * self.x) + (self.y * self.y);
        }
    }
    ```
* All the common arithmetic, comparison and logical operators. More will be implemented.
* Builtin functions.
* Memory arenas for allocating dynamic memory:
    ```py
    arena a;
    let ptr := a.new<bool>(false); # returns a pointer to a bool allocated in the arena
    let arr := a.new_array<f64>(100); # returns a span to a f64[100] array allocated in the arena
    ```
    Arenas are lexically scoped and deallocate all created objects when it goes out of scope. If a function needs to allocate objects that will outlive the function call, then a pointer to an arena should be passed into the function which it can use for allocations. Therefore pointers obtained from an arena must not outlive the arena itself. (Future challenge: static analysis to ensure this is the case).

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
   |     -- bytecode.hpp  : Definitions of op codes and utility
   |
Runtime  -- runtime.hpp   : Functionality to run a program
   |
  Output
```

# Next Features
* Complete modules
    - Namespacing
    - No transitive includes
* Complete spans
    - Create spans from other spans.
* Templates/Generics
* Filesystem Support
    - reading/readlines
* Variants

# Known Issues
* If trying to call a builtin function with an unknown type, it doesn't say that it couldn't find the function; it instead tries to lookup the function name as a variable, and reports that it couldn't find it.