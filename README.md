# anzu
An interpreted programming language written in C++. This is just me playing around with language design, it will be unsafe and inconsistent. Who knows, maybe I'll eventually make it good!

## Features so far
* Stack based like Forth.
* ints, bools and strings as types.
* Able to define variables (via `5 -> x` for example).
* `if` statements (with `else` and `elif` too).
* `while` loops.
* Functions.

## Upcoming features
* Relying less on stack-based syntax. For example, I would rather write `if (x > 5) do ... end` rather than `if x 5 > do ... end`. 
* Compile down to asm (potentially)
* Self-hosted if the above step happens
