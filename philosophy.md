# Kano Design Philosophy

I have a goal for the design of Kano. The current implementation does not meet
this philosophy, and I am working to address that.

## Language Design

Languages are complicated. Some languages are simple to implement (e.g. C), but
have some sharp edges due to generality or age. Other languages are much more
complicated to implement, but are easier to use due to strong guarantees
provided by the language (e.g. Rust).

Consider C++. C++ is a toolbox seemingly without a bottom, with several ways of
approaching each problem. The language trusts the programmer with the ability to
customise almost any aspect that they want, while giving them the tools they
need to abstract that complexity where necessary. To support this, C++ has an
enormous standard specification which unambiguously sets out the formal
semantics for the language and the standard library. These two things are
inseparable: some language features require library support (e.g. you must
include the `<initializer_list>` header before `auto` can deduce
`std::initializer_list` from the statement `auto x = {1, 2};`), while some
library features cannot be implemented without compiler help (e.g.
`std::is_empty` or `std::source_location`). This makes it very hard to use the
language without the library, or to otherwise restrict your C++ to a limited
subset without any exceptions.

I plan to address these issues in Kano while retaining some of the benefits that
come from the additional complexity. The foundation for the design is as
follows:

  * A dead-simple core language with rigorously defined formal semantics. This
    would be as simple as C, possibly even simpler, with the goal of providing
    a portable low-cost abstraction over modern hardware. The language core
    consists only of features which cannot possibly be implemented in terms of
    other core features. That is, the core is as big as necessary, but no
    bigger.
  * A collection of syntactic abstractions, i.e. syntax sugar. These can range
    in complexity from trivial transformations (e.g. type literals) to complex
    rewrites (e.g. rewriting coroutine direct style to continuation passing
    style). The syntax sugar must be defined strictly as a translation from one
    legal Kano program to another. That is, any syntactic construct defined as
    syntax sugar must be exactly equivalent in semantics to some other construct
    in the core language. This property allows the programmer to de-sugar their
    code to see exactly what is going on underneath the surface.

## Core Abstractions

`Kano Core` will model some familiar abstractions, but depart from others.

  * No exceptions: all control flow is explicit.
  * No RAII: RAII is a posterchild for syntax sugar, and is much more suitably
    defined as a transformation on top of the core.

### Function Call Frame Allocation

In many languages, functions implicitly allocate their frames using a stack.
This abstraction is transparent for most programs, but the implementation detail
leaks in certain circumstances. For instance, when too many recursive function
calls occur, the stack can be exhausted. Another example is in asynchronous
programming: we can no longer maintain function state in the stack because the
function state would be destroyed when we suspend and no longer be available
when we want to subsequently resume.

Kano is removing this abstraction and substituting it with explicit frame
allocation.

```
function square(x) {             | function square(x) {
  return x * x;                  |   return x * x;
}                                | }
function f(a, b) {               | function f(a, b) {
  // No explicit syntax for      |   // Space for square can be allocated
  // allocating the recursive    |   // within the frame for f.
  // call frames.                |   var s : square;
  return square(a) + square(b);  |   // We can reuse the same space for
}                                |   // both calls since they are not
                                 |   // concurrent.
                                 |   return s(a) + s(b);
                                 | }
function factorial(n) {          | function factorial(n) {
  if (n < 2) {                   |   if (n < 2) {
    return 1;                    |     return 1;
  } else {                       |   } else {
    // Syntax is the same for    |     // Space for the recursive call
    // function calls that may   |     // cannot be allocated directly
    // have unbounded depth.     |     // within the frame: the frame
    return n * factorial(n - 1); |     // size must be a constant so it
  }                              |     // cannot contain itself.
}                                |     var f : *factorial = new factorial;
                                 |     var result : int = n * f(n - 1);
                                 |     delete f;
                                 |     return result;
                                 |   }
                                 | }
```

Obviously, the code on the right hand side involves more programmer effort than
the code on the left hand side. However, there are reasons why the right hand
side is more desirable.

Firstly, this method for handling call frames makes the transition to stackless
coroutines much more gentle. With stackless coroutines, the call frame for the
function can outlive the initial execution. With an implicit stack, this means
that the frame is transparently being treated differently. In the case of C++,
this results in heap allocations behind the scenes. There are limited cases
where Heap Allocation eLision Optimization (HALO) can apply and remove the
expensive allocations, but this is a quality of implementation issue and not
something that is guaranteed by the standard. With the explicit allocation
approach shown here, such allocation is completely under the control of the
programmer all the time. The programmer can apply domain-specific knowledge to
decide how and where to allocate frames in order to meet their requirements.

Secondly, this allocation requirement increases clarity for the cases where the
stack abstraction leaks: if one was to write an infinitely recurring function in
the conventional stack-allocating style, there would be nothing in the code that
would indicate that the depth was unbounded without the programmer carefully
reading the whole control flow. With explicit frame allocation, if the
programmer can see that all frames are allocated directly inside the caller's
frame, it is obvious that the total stack depth is bounded by construction. This
is nice for guaranteeing safety and correctness in constrained environments.

### Modules

Kano modules aim to meet the following objectives:

  * Allow Kano programs to scale beyond a single file, promoting code reuse
    through libraries (duh).
  * Provide resilience to changes: changes to libraries should not break code
    that uses those libraries unless the changes are to the specific features
    used in the code. In particular, it should not be possible for a new name in
    the library to break the code of the importer.
  * Provide intuitive human-readable name lookup semantics that make it easy for
    a programmer to find the source for a given name without access to
    sophisticated tooling.
  * Allow for a relaxed mapping between the logical layout of import targets and
    the physical layout of source on disk.

Modules may `export` definitions by prefixing the definition with the `export`
keyword. Modules may `import` other modules to gain access to the exported
definitions from that module. The non-exported definitions within a module
cannot be imported by other modules.

Modules have names. The names consist of dot-separated sequences of identifiers,
such as `std.io`. When importing a module `foo.bar.baz` from within the module
`foo.bang`, the module to import can be named either absolutely (e.g. `import
foo.bar.baz;`) or relatively (e.g. `import .bar.baz;`). Relative imports are
resolved by prepending the preceding path component of the current module,
excluding the base module name (i.e. `foo.bang` -> `foo`).

The mapping from module names to file paths is derived from two parts: the
module root and the module path. The module root is the first identifier in the
absolute module name (e.g. `foo.bar.baz` has the root `foo`), while the module
path is the remainder of the module name. Module roots are mapped to directory
paths via compiler flags. Module paths translate into relative paths under the
module root path. For instance, if the root `foo` is defined as
`/usr/kano/foolib`, the module `foo.bar.baz` will map to
`/usr/kano/foolib/bar/baz.kano`.

### const semantics

There are a few different meanings for `const` as it appears in C++:

```
const int x = 1;        // A global constant.
void f(const int& x) {  // A const reference.
  const int y = x;      // A local constant.
}
```

These different kinds of const have different semantics:

  * A global constant is truly constant: the value in this memory location will
    never change throughout the program execution. Often, such constants will be
    placed in memory that is marked as read-only, preventing them from being
    accidentally changed and allowing the same physical pages to be shared
    between different instances of the same process.
  * A local constant is also constant for the duration of its life, but since
    the lifetime of the constant can end before the program terminates, and
    since we may reuse the same memory location for different variables at
    different times, this kind of constant is not mapped in read-only memory.
  * Finally, a const reference is not actually a constant: it's a contract.
    Consumers of a const reference may not modify the object that is referenced.
    However, this does not mean that the referenced object is constant. This
    precludes optimizations that would require us to assume that the underlying
    object does not change over time. In C++, there is no way of signalling that
    a pointer refers to an object which is truly constant.

Kano handles these cases separately and tries to cover some of the pitfalls:

  * Global constants are part of Kano Core. This is essential for allowing such
    constants to be mapped in readonly pages.
  * Local constants are sugar: the compiler will check for const correctness and
    translate the variables into mutable ones with pointer casts.
  * Kano has both `const` and `readonly` keywords, both implemented as syntax
    sugar. `*const int` is a pointer to an int which is guaranteed not to change
    throughout the duration of the lifetime of the pointee, while `*readonly
    int` is a pointer to an int which we are not allowed to use to mutate the
    pointee. A `*const int` can implicitly cast to a `*readonly int` but not
    vice-versa. You cannot have a `readonly int`, that is just a `const int`.

## Syntactic Abstractions

Kano is likely to adopt various abstractions:

### Statement Expressions

Similar to the GCC compiler extension of the same name, statement expressions
allow you to use arbitrary statements inside the evaluation of an expression:

```
var x : int = ${       | var x : int;
                       | {
  var ff : f;          |   var ff : f;
  var gf : g;          |   var gf : g;
  a + b;               |   x = a + b;
};                     | }
```

This is a building block for many other abstractions.

### Implicit Function Frame Allocation

As outlined above, Kano does not have an implicit stack. In the case where the
inner function call frame size does not depend on the size of the current stack
frame (e.g. simple function composition without recursion), Kano can allow for
call frames to be implicitly created at call sites instead of requiring them to
be specified manually:

```
function square(x) {             | function square(x) {
  return x * x;                  |   return x * x;
}                                | }
function f(a, b) {               | function f(a, b) {
  // No explicit syntax for      |   // Space for square can be allocated
  // allocating the recursive    |   // within the frame for f.
  // call frames.                |   var s : square;
  return square(a) + square(b);  |   // We can reuse the same space for
}                                |   // both calls since they are not
                                 |   // concurrent.
                                 |   return s(a) + s(b);
                                 | }
function factorial(n) {          |
  if (n < 2) {                   |
    return 1;                    |
  } else {                       |
    // Compile error: factorial  |
    // frame cannot be allocated |
    // inside itself, so it must |
    // be allocated explicitly.  |
    return n * factorial(n - 1); |
  }                              |
}                                |
```

### Templates

C++ templates are powerful. Kano is likely to adopt some form of templates with
similar semantics. A template gives a set of instructions for how to produce
a specialised version of something for a given set of template parameters. Upon
use of the template, the instantiations are implicitly created.

Using `` `backticks` `` to expositionally represent names which the programmer
cannot type themselves, template expansion would operate approximately like so:

```
template function f<typename T>(  | function `f<int>`(x : int) : int {
    x : T) : T {                  |   return x * x;
  return x * x;                   | }
}                                 |
                                  | function `f<double>`(x : double) : double {
                                  |   return x * x;
                                  | }
                                  |
function demo() : void {          | function demo() : void {
  print(f(2));                    |   print(`f<int>`(2));
  print(f(2.0));                  |   print(`f<double>`(2.0));
}                                 | }
```

### Monadic Error Handling

In Rust, one can use `?` to monadically handle errors. A similar facility is
likely to be adopted for Kano:

```
function f() : result<int> {      | function f() : result<int> {
  ...                             |   ...
}                                 | }
                                  |
function g() : result<double> {   | function g() : result<int> {
  var x : int = f()?;             |   var x : int = ${
                                  |     var result : result<int> = f();
                                  |     if (!result.ok()) {
                                  |       return result.error();
                                  |     }
                                  |   };
  return 2.0 * double{x};         |   return 2.0 * double{x};
}                                 | }
                                  |
```

### Stackless Coroutines

Stackless coroutines allow for writing asynchronous code in a direct style
without imposing the overhead of a stack per execution context. The rewrite is
non-trivial (although it is made simpler by the explicit call frame allocation
described above).

TODO: Figure out the minimum core support change needed to allow coroutines and
add an example of the transformation.
