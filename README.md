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
    1. Null pointers: pointers can be created from, and compared to, `null`. It is an error to derefence one.

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
    1. Null spans: spans can be created from, and compared to, `null`. It has a size of zero.

* Function Pointers:
    1. Function names resolve to function pointers which can be passed to functions.
    1. Syntax for function pointer types: `fn(<arg_types>) -> <return_type>`.

* Variables:
    * Declare with `:=` operator and either `let` or `var`: `let x := 5` or `var x := 5`.
    * `let` declares a const value and `var` declares a mutable value.
    * Assign to existing variable with `=` operator: `x = 6`.

* Comments using `#` symbol.

* `if` statements.

    ```
    if <condition> {
        ...
    } else if <condition> {
        ...
    } else {
        ...
    }
    ```

* basic `loop` loops (with `break` and `continue`):
    ```
    loop {
        ...
    }
    ```

* `while` loops:

    ```
    while <condition> {
        ...
    }
    ```

* `for` loops (for arrays only for now):

    ```
    for <name> in <array> {
        <body>
    }
    ```

* `fn` function statements:

    ```
    fn factorial(i: u64) -> u64 {
        if (i == 0u) {
            return 1u; 
        }
        return i * factorial(i - 1u);
    }
    ```
* `struct` statements and member functions:
    ```
    struct vec2
    {
        x: f64;
        y: f64;

        fn length2(self: const&) -> f64
        {
            return (self.x * self.x) + (self.y * self.y);
        }
    }
    ```
    * The type of the arg for a member function does not need to be explicitly specified.
    * If the first arg is not a pointer to the type, it is classed as a "static" method and only callable from the type.

* All the common arithmetic, comparison and logical operators.

* Type casting with `as`.
    Currently only `x as i64` and `x as u64` is supported where `x` is a fundamental type.

* Builtin functions
    This is a somewhat rubbish attempt at implementing C functions. I will probably remove this and reimplement from scratch when I figure out how.

* Intrinsic "functions"
    These are operators for accessing compiler internals or to perform operations that require specialised op codes in the runtime to be efficient. They are prefixed with a `@`.
    They are more flexible than functions; some accept types as arguments and you can call some of them in places where functions can't, eg `@type_of` can be called anywhere a type is expected. You cannot take the address of an intrinsic or assign them to variables.
    * `@len(obj)` behaves differently depending on the object. If it's an array or span, returns the number of elements. If it's an arena, is returns the number of bytes allocated. If it's a struct that has a `.len() -> u64` member function, it calls that. Otherwise it's a compiler error.
    * `@size_of(x)` returns the size in bytes of the type of object `x`. `x` can also be itself a type.
    * `@type_of(x)` returns the type of `x`. Can be used anywhere a type is expected.
    * `@type_name_of(x)` returns a string representation of the type of `x`.
    * `@copy(dst, src)` takes two spans of the same type and copies the contents of one into the other. The size of `dst` must be big enough to fit `src`, otherwise it's a runtime error. This exists because it can efficiently memcpy the data rather than looping over the elements.
    * `@compare(lhs, rhs)` takes two pointers of the same type and compares them bytewise via memcmp. 
    * `@import(name)` for importing and using other modules (more info below). This can only be used in the global scope.
    * `@fn_ptr(func)` takes the name of a function an explicitly converts it to a function pointer.
    * `@is_fundamental(type)` returns `true` (compile time bool) if the given type of one of the builtin types.
    There's no reason why these couldn't be keywords (like how `sizeof` is a keyword in C++); there's no real criteria for what should be a keyword, but some of these seem too niche to be classed as its own language feature (`type_name_of` feels wrong being a keyword for example) and for others I just like this style more (`@import` feels better to me that just a plain `import`)

* Memory arenas for allocating dynamic memory
    ```
    arena a;
    let ptr := new(a) false; # returns a pointer to a bool allocated in the arena
    let arr := new(a, 100) 0u; # returns a span to a f64[100] array allocated in the arena
    ```
    Arenas are lexically scoped and deallocate all created objects when it goes out of scope. If a function needs to allocate objects that will outlive the function call, then a pointer to an arena should be passed into the function which it can use for allocations. Therefore pointers obtained from an arena must not outlive the arena itself. (Future challenge: static analysis to ensure this is the case).

* Templates functions
    ```
    fn foo!(T)(x: T, y: T) -> T { ... }

    let x := foo!(i64)(2, 3);
    ```
    * C++ and D style templates using D style syntax. The syntax is a bit odd and I would have preferred `foo<i64>` or `foo|i64|`, but those add a lot of complexity to the parser. the `!` token is needed to keep parsing simple.
    * Member functions can also be templated.

* Modules
    ```
    let vec := @import("lib/vector.az");
    var my_vec := vec.vector!(u64).create(alloc&);
    ```
    * Import other files and access their contents via the defined module object.
    * Global variables, structs and functions are made available.


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
* Hash Maps
* Generators
* Pattern Matching
* Variants