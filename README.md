Sergio's C compiler
===================

This is a compiler for a strict superset of the C language. Specifically, C99.
The goal of this project is to create a simple compiler that fully supports C99
but that also adds new features to the language.

List of features.
-----------------

All of C99 plus:

- Namespaces and a module system
- Structs with optimal layout
- Native SIMD types a la Intel ISPC
- New preprocessor
- Reflection
- Operator overloading