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

Example program:

    function fib(n : integer) : integer {
      var a : integer = 0;
      var b : integer = 1;
      var i : integer = n;
      while (i) {   # a = fib(n - i), b = fib(n - i + 1)
        i = i - 1;  # a = fib(n - i - 1), b = fib(n - i)
        b = a + b;  # b = fib(n - i - 1) + fib(n - i)
                    #   = fib(n - i + 1)
        a = b - a;  # a = fib(n - i + 1) - fib(n - i - 1)
                    #   = fib(n - i)
      }
      # i = 0 -> a = fib(n)
      return a;
    }

## Bup

A portmanteau of "bottom up", so named because it is the backend for the
compiler. Bup exposes a stack machine instruction set and compiles to x86
assembly.

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
