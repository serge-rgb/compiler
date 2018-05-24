scc
===================

This is a compiler for a strict superset of the C language.



List of features.
-----------------

- All of C11
- Very short compile times
- A module system
- Structs with optimal layout (auto struct Foo {  };  )
- Native SIMD types a la Intel ISPC
- Reflection (compiler-provided type info)
-  print enum values as strings
- C++ decltype (typeof keyword ala GCC extension.)
- Runtime automatic optional NULL dereference check.
- Array bounds checking whenever possible.
- C++ auto
- else after while
- Variable-sized array (does C11 already have this?)

Maybe... :
- lax type compatibility rules for anonymous structs
- declarations inside if
- "Fat" pointers (lang support for pointers + lengths)
- New preprocessor
- Type dispatch
