scc
===

This is a work in progress. Still in the extremely-early stages.
----------------------------------------------------------------

The current goal of this project is to implement (most of) C, with some
deviations which can be opted-out of, plus some new features.

Parts of C17 that won't be implemented.
---------------------------------------
- Atomics
- Complex numbers
- The standard library

Deviations from the C spec.
---------------------------
- Nn `->` operator. Pointers to structs can be accessed with `.`.
- `struct` has an implicit typedef
- Zero initialization by default


New features
------------
- Wide types and SIMD operators
- Reflection strings for `enum`
- `decltype`
- C++ style auto
- File-system based module/build system
- Namespaces
- Function overloading
- Memory checking a-la address sanitizer
- `else` after `while`

Targets
-------
- x64 on Windows and macOS
- ARM on macOS
- LLVM IR
- SPIR-V

Dependencies
------------
- On Windows: MSVC (tested with Visual Studio 2017)
- On Mac/Linux: Clang

FAQ
===

What is this?
-------------

scc is a C compiler that plays it loose with the spec.

When?
-----

I don't know. As long as I keep a non-zero pace and manage not to die before
it's done then time is on my side.


What are your sources and references?
-------------------------------------

My main sources of documentation have been

- The [C11 spec](https://port70.net/~nsz/c/c11/n1570.pdf) .
- The [Intel  Software Development Manual] (https://software.intel.com/en-us/articles/intel-sdm)
- [Retargetable C Compiler](https://www.pearson.com/us/higher-education/program/Hanson-Retargetable-C-Compiler-A-Design-and-Implementation/PGM166351.html), mostly for implementing the front end. For code generation I have used the following:
- Code generation is 100% based on [DCCG](https://www.cs.indiana.edu/~dyb/pubs/ddcg.pdf), which I found out thank's to Fabian Giesen's [papers I like](https://fgiesen.wordpress.com/2017/08/12/papers-i-like-part-1/)
- On extreme circumstances I might peek at Fabian Bellard's [tcc](https://bellard.org/tcc/tcc-doc.html) implementation. However, this is considered cheating and is left as an absolute last measure.
- On a couple of occasions I have turned to [Agner Fog](http://www.agner.org/optimize/), but the code generator is still very simple and almost no effort has been spent on actually outputting decent code.

