# Kano

From the Greek "κάνω", meaning "do".

  * The syntax is inspired by TypeScript syntax, e.g:

        function fib(n : integer) : integer {
          ...
        }

  * Type syntax is inspired by Go, e.g:

        var x : integer;
        var y : *integer = &x;

  * Semantics will be similar to C, e.g. plain data structures, value semantics.

Ideas for future features:

  * Multi-file support with module name and file layout similar to Python, but
    with explicit export statements in modules.
  * Unbounded integer types. The compiler will perform static analysis to
    determine the bounds of all integer expressions and generate code that can
    handle that range. The expectation is that almost all real code will have
    small provable bounds, but we shall see...
  * Allocate stack frames just like structs: a stack frame must have space for
    all recursive calls it makes. If the compiler can't prove and determine
    a fixed frame size, frames must explicitly be heap-allocated. This makes it
    trivial to track stack growth, makes stack explosion due to recursion have
    obvious syntax, and sets the groundwork for nifty things like zero overhead
    coroutines.

Example program:

    function fib(n : integer) : integer {
      var a : integer = 0;
      var b : integer = 1;
      var i : integer = 0;
      while (i < n) {  # a = fib(i), b = fib(i + 1)
        i = i + 1;     # a = fib(i - 1), b = fib(i)
        b = a + b;     # b = fib(i - 1) + fib(i) = fib(i + 1)
        a = b - a;     # a = fib(i + 1) - fib(i - 1) = fib(i)
      }                
      # i == n -> a = fib(n)
      return a;
    }
    
    function main() : integer {
      return fib(10);
    }

## Bup

A portmanteau of "bottom up", so named because it is the backend for the
compiler. Bup exposes a stack machine instruction set and compiles to x86
assembly.

Ideas for future features:

  * Support for objects larger than a register (copying, function parameters,
    return values).
  * Arbitrary (but static) sized integer arithmetic.

Example program:

    message = ascii "Hello, World!\n"

    function _start 0 {
      # write(stdout, message, sizeof(message))
      const 14
      address message
      const 1
      builtin write
      call 3
      # exit(0)
      const 0
      builtin exit
      call 1
    }
