# anzu
An interpreted programming language written in C++. This is just me playing around with language design, it will be unsafe and inconsistent. Who knows, maybe I'll eventually make it good!

This started out a stack based language like Forth. Meaning that users would have direct access to a stack where they could push values and operations to perform actions. For example, printing the sum of 4 and 5 would be written as

```
4 5 + .
```
First, 4 and 5 are pushed to the stack, then `+` pops two elements off, adds them and pushes the return value, then `.` prints to the console. I could do a lot with this, and implemented basic control flow via `if`, `elif`, `else`, `while`, `break`, `continue` and `end`, as well as functions. This had the benefit of being relatively simple to implement since it didn't require an abstract syntax tree.

The parser would then turn the code into a vector of op codes which somewhat resemble a bytecode that could potentially be turned into assembly code in the future.

I have since added an AST on top of this and disallowed direct stack access from the code, allowing for syntax that looks similar to python. All of this translates back down to the same stack-based-op-code approach under the hood, which I have since found out is exactly how python works, which is quite cool. The above example now looks much more familiar:

```
print(4 + 5)
```

## Features so far
* Supports `ints`, `bools`, `floats`, `null` and `string-literals`.
* Assignment to variable names in the expected way: `x = 5`.
* `if` statements (with optional `else` and `elif` too).

    ```
    if <condition> {
        ...
    } else if <condition> {
        ...
    } else {
        ...
    }
    ```
* `while` loops (with optional `break` and `continue`):

    ```
    while <condition> {
        ...
    }
    ```
* `for` loops (with optional `break` and `continue`):

    ```
    for <variable_name> in <list_object> {
        ...
    }
    ```
* `function` statements (with optional `return`):

    ```
    function <name>([<arg>: <type>]*) -> <return_type> {
        ...
    }
    ```
* All the common arithmetic, comparison and logical operators. More will be implemented.

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
   |     -- typecheck.hpp : Type checks expressions and function defs/calls.
   |     -- optimiser.hpp : Modifies an AST to produce a smaller, equivalent AST
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
-- print.hpp       : Wrapper for std::format, similar to {fmt}
-- peekstream.hpp  : A data structure used in the lexer
-- overloaded.hpp  : A helper class to make std::visit simpler
-- score_timer.hpp : An RAII class for timing a block of code
```

# Upcoming Features
* Const keyword.
* Member functions so we can write `my_list.size()` rather than `list_size(my_list)`.
* Function overloading based on the call signature.
* Replace `int` with `int32`, `int64` as well as promotion/narrowing builtins.
* Add `float32` and `float64`, with promotion/narrowing builtins (and to/from ints).
* Add `uint32` and `uint64`, similar to the above.
* Custom types.
* Removal of objects and types from the runtime, should run on arrays of bytes.
* Native compilation.
* References (like C++, no pointers).
* Less restriction on return statements in functions.
* Typed function pointers.
* Variants and a basic match statement.
* Replacing the binary operation op codes with calls to builtin functions.