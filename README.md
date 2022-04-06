# anzu
An interpreted programming language written in C++. This started out as a stack-based language like Forth, it then took a route similar to Python with structures programming and duck-typing, and now I am drifting towards it being more simiar to C with a lot of "compile-time" checks including static typing.

## Features so far
* Supports `ints`, `bools`, `null` and `string-literals`.
* Variables:
    * Declare like `x := 5`.
    * Assign a new value like `x = 6`.
    * This syntax makes updating a variable in an outer scope vs a variable in the local scope unambiguous.
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
* `fn` function statements (with optional `return`):

    ```
    fn <name>([<arg>: <type>]*) -> <return_type> {
        ...
    }
    ```
* `struct` statements:
    ```
    struct <name>
    {
        <field_name> : <field_type>
        ...
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

# TODO
## Focus
* Scopes
* Function overloading
* Member Functions
* Constructors
* Const
* Dynamic Allocation

## Low Priority
* Making the bytecode a true bytecode that can be saved to a file and read back in, with the compiler and runtime becoming separate programs
* References
* Less restriction on return statements in functions
* Function pointers
* Variants and match statement
* Enums
* Filesystem support
* A better C++ API for implementing custom functions in C++