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
    if <condition> do
        ...
    elif <condition> do
        ...
    else
        ...
    end
    ```
* `while` loops (with optional `break` and `continue`):
    ```
    while <condition> do
        ...
    end
    ```
* `for` loops (with optional `break` and `continue`):
    ```
    for <variable_name> in <list_object> do
        ...
    end
    ```
* `function` statements (with optional `return`):
    ```
    function <name>(<args>) do
        ...
    end
    ```
* All the common arithmetic, comparison and logical operators. More will be implemented.

# Upcoming Features
* Static type checking
* Maps (dictionaries)
* Better support for functions, function pointers?
* Classes
* More operations when needed.
