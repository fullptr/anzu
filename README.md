# anzu
An interpreted programming language written in C++. This started out as a stack-based language like Forth, it then took a route similar to Python with structures programming and duck-typing, and now I am drifting towards it being more simiar to C with a lot of "compile-time" checks including static typing.

## Features so far
### Fundamental types
* Signed integral types `i32` and `i64`.
* Unsigned integral type `u64`.
* Floating point type `f64`.
* Boolean type `bool`.
* Character type `char`.
* Null type `null`.

### Pointers
* Uses trailing syntax for both taking addresses and dereferencing.
* For example: `i64&` is an `i64` pointer.
* If `ptr` is an `i64&`, then `ptr@` is the int that it points to.
* Uses `@` instead of the familiar `*` because using `*` in trailing syntax can be ambigious *thmultiplication. Plus I like it more; it signals that I'm using the value "at" the pointer.
* Null pointers: pointers can be created from, and compared to, `null`. It is an error to derefence one.

### Arrays
Fixed size arrays with statically known size. Spelled `T[N]` where `T` is a type and `N` is a `u64`.
* Declare elements up front: `l := [1, 2, 3]`.
* Declare repeat value and size: `l := [0; 5u]` (same as `l := [0, 0, 0, 0, 0]`).
* All objects in an array must be the same type.

### Spans
* Non-owning views over arrays, made up of a pointer + a size.
* Create a span from an array using trailing `[]`.
* eg: If `l` is an array of 5 `i64`s, then `l[]` is an `i64[]`.
* Slicing syntax `l[0 : 2]` for creating subspans.
* Arrays can automatically convert to spans when passing to functions.
* Null spans: spans can be created from, and compared to, `null`. It has a size of zero.

### Function Pointers
Function names can be converted to function pointers which can be passed to functions.
* Syntax for function pointer types is `fn(<arg_types>) -> <return_type>`.

### Const
* Add `const` to a type as a suffix just like pointers and spans.
* Example: `i64 const` is a constant `i64`, `i64 const[]` is a span of `i64 const`, and `i64[] const` is a const span pointing to mutable ints.

### Variables
Declare with `:=` operator and either `let` or `var`: `let x := 5` or `var x := 5`.
* `let` declares a const value and `var` declares a mutable value.
* Explicit typing can be provided: `let x : i64 = 10`. Safe type conversions can happen here (see more info below).
* Assign to existing variable with `=` operator: `x = 6`.

### Comments
Uses the `#` symbol.

### `if` Statements
```
if <condition> {
    ...
} else if <condition> {
    ...
} else {
    ...
}
```

### `loop` Statements
An infinite loop. Permits `break` and `continue` like other languages.
```
loop {
    ...
}
```

### `while` Statements
```
while <condition> {
    ...
}
```

### `for` Statements
Only implemented for spans. `name` is a pointer to the current element in the array.
```
for <name> in <span> {
    ...
}
```

### Functions
Declared with the keyword `fn`.
```
fn factorial(i: u64) -> u64
{
    if (i == 0u) {
        return 1u; 
    }
    return i * factorial(i - 1u);
}
```

### Structs
Declared with the keyword `struct`.
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
Structs can have nested functions. If the first argument is a pointer to an instance of the struct, then it can be called as a member function. Otherwise, it is a "static" function and can only be invoked directly on the class.

Further, for member functions, the type does not need to be explictly typed, you only need to write `&` or `const&`.

### Arithmetic, Comparison and Logical Operators
* `+`, `-`, `*`, `/`, `%`, `<`, `<=`, `>` and `<=` are implemented for the numeric builtin types.
* `==` and `!=` implemented for all builtin types.
* `||` and `&&` are implemented for `bool`, and short circuit.

### Compile Time Bools
A fairly half baked feature with some potential. Currently only implemented to enable comparing types, a useful feature for templates. For example, if `T` is the template type, checking if it is an `i64` is as simple as `T == i64`.

Further, if the condition for an `if` statement is a compile time bool, only the true branch gets compiled and the condition doesn't exist at runtime. Equivalent to C++ `if constexpr` expressions.

It feels like `true` and `false` themselves should be compile time bools, but then expressions like `let x := true` would make the type of `x` be a compile time true, so you would need to write `let x : bool = true`. Taking things further, all literals of all fundamentals could be compile time too, but the complexity of the implementation would be massive.

These are an exapmle of a "size zero" type, more on that below.

### Safe Type Conversions
There are various safe conversions between some types that can implicitly happen in variables declarations and function calls:
```
fn foo(x: i64&) { ... }

foo(null); ## null auto-converts to a null pointer of type i64&
```
These are
* Non-const objects can convert to const objects.
* `null` can convert to any pointer type, resulting in a null pointer.
* `null` can convert to any span type, returning in a null span of size 0.
* Function types can convert to function pointer types.
* Compile-time bools can convert to regular bools.

### Unsafe Type Conversions
Currently only `x as i64` and `x as u64` is supported where `x` is a fundamental type.

### Builtin Functions
This is a somewhat rubbish attempt at implementing C functions. I will probably remove this and reimplement from scratch when I figure out how.

### Intrinsic "Functions"
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

### Memory Arenas
Anzu's way of handling dynamnic memory allocations.
```
arena a;
let ptr := new(a) false; # returns a pointer to a bool allocated in the arena
let arr := new(a, 100) 0u; # returns a span to a f64[100] array allocated in the arena
```
Arenas are lexically scoped and deallocate all created objects when it goes out of scope. If a function needs to allocate objects that will outlive the function call, then a pointer to an arena should be passed into the function which it can use for allocations. Therefore pointers obtained from an arena must not outlive the arena itself. (Future challenge: static analysis to ensure this is the case).

### Template Functions
C++ and D style templates using D style syntax. The syntax is a bit odd and I would have preferred `foo<i64>` or `foo|i64|`, but those add a lot of complexity to the parser. the `!` token is needed to keep parsing simple.
```
fn foo!(T)(x: T, y: T) -> T { ... }

let x := foo!(i64)(2, 3);
```
Structs and member functions can also be templated
```
struct foo!(T) { ... }
```
Template objects themselves can be called directly with an argument list; the template types get deduced from the arguments. If this deduction fails, it is a compile time error. Safe type conversions don't apply here; the arguments must match the placeholders completely.

### Modules
Import other files and access their contents via the defined module object. Global variables, structs and functions are made available.
```
let vec := @import("lib/vector.az");
var my_vec := vec.vector!(u64).create(alloc&);
```

### "Size Zero" Types
Many compile time objects are represented in Anzu's type system, but have no runtime information since all their info is contained in their type. This results in types that are not particularly useful, but does have some nice quirks.

For example, if I had `struct foo { x: i64; }`, then `foo` itself is an object of size zero, whose type is `<type: foo>`. A constructor call then, is simply implemented as the call operator on this object which returns an object of type `foo`. This then naturally allows you to create type aliases with the normal variable syntax: `let f := foo` creates a variable `f` of type `<type: foo>`, so calling it is just a constructor call for `foo` as if you had used `foo` directly.

Just like how every struct definition is an object of its own type (for every type `T` there is the type `<type: T>`), the same applies to functions. `let f := func` gives a new name to the function, and `f` is a function type and not a function pointer type. To create a function pointer explicitly from a function, you can either declare the function pointer type in the declaration to make a safe type conversion happen, or use the more convention `@fn_ptr` intrinsic (`let f := @fn_ptr(func)`).

Some more "size zero" types are:
* Functions
* Structs
* Modules
* Function Templates
* Struct Templates
* Bound Methods
* Compile Time Bools
* Types themselves (the type of `i64` is `<type: i64>`)

"Calling" a struct template with a template list results in a concrete struct, and calling that yields an instance of the struct.

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