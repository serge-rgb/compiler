scc - Sergio's C compiler
====================================

This is a compiler for a strict superset of the C
language. Specifically, C99.  The goal of this project is to create a
simple compiler that fully supports C99 but that also adds new
features to the language.

List of features.
-----------------

All of C99 plus:

- Namespaces and a module system
- Structs with optimal layout
- Native SIMD types a la Intel ISPC
- New preprocessor
- Reflection
- Type dispatching
- Operator overloading
- Runtime automatic optional NULL dereference check.
- Array bounds checking whenever possible.
- Runtime pointer length info
- C++ decltype
- C++ auto
- else after while
- defer
- Variable-sized array


High Level Action Plan
----------------------

- Finish adding block-scope declarations to the code generator.
- One by one, add the rest of the C int expressions.
- Emit HTML for ASM. Every line a hyperlink to the codegen source.
- Add conditional expressions.
