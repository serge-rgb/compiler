scc: the fANSI C compiler
=========================


This is a work in progress. Still in the extremely-early stages.
----------------------------------------------------------------


Things that are (partially) implemented as of June 12, 2018:
------------------------------------------------------------

- functions (function definition / function call. no variable arguments yet)
- pointers
- ints / chars,
- structs
- win64 ABI
- system V (e.g. Mac & linux) ABI .
- assignment expressions. (=, +=, -=)
- arithmetic expressions on ints. (+, -, *)
- declarations for stack variables

Deviations from the C spec.
---------------------------
- No `->` operator. Pointers to structs can be accessed with `.`.
- Laxer type compatibility rules. (types are compatible if their fields match)

To-Do (short term):
-------------------
- machine abstraction
- prefix addition & subtraction (++i, --i)
- floating point
- typedefs
- arrays
- external functions
- global declarations
- preprocessor
- "Legacy" mode for including and/or writing standard C.

To-Do (mid term):
------------------
- wide types
- structs with automatic field reordering
- array bounds checking whenever possible.
- C++ decltype (typeof keyword ala GCC extension.)
- C++ auto
- else after while
- variable-sized array (does C11 already have this?)

To-Do (long term):
------------------
- better codegen (i.e. actually reading papers and implementing multiple passes)
- a module system (or at least something better than #include)
- hand-written assembler (drop nasm dependency)
- hand-written linker (drop link.exe dependency)

Maybe, maybe not
----------------
- llvm or gcc backend (for when the-best-codegen is a necessity)
- reflection (compiler-provided type info)
- declarations inside if
- "fat" pointers (lang support for pointers + lengths)
- new preprocessor
- type dispatch

FAQ
---

What is this?
-------------

scc is a C compiler that plays it loose with the spec.


Why?
----

I want to write a language that is close in spirit to the original C. That is,
a language that has a clear correspondence between code and machine
instructions. This means adding support for wide types and some mechanism of
expressing control flow with them. [ISPC](https://ispc.github.io/) is a huge
inspiration for this, but I plan on attacking the problem from a more explicit
angle, which may end up meaning lots of intrinsics, which are not technically a
language feature. Not because I think ISPC's approach is wrong, but rather I
want to try something different and see where it goes.

When?
-----

I don't know. As long as I keep a non-zero pace and manage not to die before
it's done then time is on my side.


What are your sources and references?
-------------------------------------

My main sources of documentation have been

- The [C11 spec](https://port70.net/~nsz/c/c11/n1570.pdf) .
- The [Intel  Software Development Manual] (https://software.intel.com/en-us/articles/intel-sdm)
- [Retargettable C Compiler](https://www.pearson.com/us/higher-education/program/Hanson-Retargetable-C-Compiler-A-Design-and-Implementation/PGM166351.html), mostly for implementing the front end. For code generation I have used the following:
- Code generation is 100% based on [DCCG](https://www.cs.indiana.edu/~dyb/pubs/ddcg.pdf), which I found out thank's to Fabian Giesen's [papers I like](https://fgiesen.wordpress.com/2017/08/12/papers-i-like-part-1/)
- On extreme circumstances I might peek at Fabian  Bellard's [tcc](https://bellard.org/tcc/tcc-doc.html) implementation. However, this is considered cheating and is left as an absolute last measure.
- On a couple of occasions I have turned to [Agner Fog](http://www.agner.org/optimize/), but the code generator is still very simple and almost no effort has been spent on actually outputting decent code.

