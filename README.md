scc - C with Cheese
===================

This is a compiler for a strict superset of the C language. Specifically, C11.
The goal of this project is to create a simple compiler that fully supports C11
but that also adds new features.


List of features.
-----------------

- Very short compile times

All of C11 plus:

- Namespaces and a module system
- Structs with optimal layout (auto struct Foo {  };  )
- Native SIMD types a la Intel ISPC
- Reflection (compiler-provided type info)
-  print enum values as strings
- C++ decltype (typeof keyword ala GCC extension.)
- Runtime automatic optional NULL dereference check.
- Array bounds checking whenever possible.
- C++ auto
- else after while
- declarations inside if
- defer
- Variable-sized array (does C11 already have this?)

Maybe... :
- "Fat" pointers (lang support for pointers + lengths)
- New preprocessor
- Type dispatch
- Operator overloading


High Level Action Plan
----------------------

- Find a good C unit testing tool.
- Finish adding block-scope declarations to the code generator.
- One by one, add the rest of the C int expressions.


