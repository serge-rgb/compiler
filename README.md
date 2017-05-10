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


High Level Action Plan
----------------------

- One by one, add the rest of the C int expressions.
- Blocks
- Declarations.
- Emit HTML for ASM. Every line a hyperlink to the codegen source.
- Add conditional expressions.


Notes
-----

What happens if we run out of registers?
What is register spilling?

You get the current scope, and you add up the size that you need to
cover all the stack variables. Then you take into account all the
registers that spill onto the stack.

Block objects keep track of their children and adjust their stack
requirements based on them.

Register spills are handled by a spill area within the block object
stack space.
