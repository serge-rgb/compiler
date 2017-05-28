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
- Variable-sized array


High Level Action Plan
----------------------

- One by one, add the rest of the C int expressions.
- Blocks
- Declarations.
- Emit HTML for ASM. Every line a hyperlink to the codegen source.
- Add conditional expressions.


Notes
-----

-- 2017-05-24

Right now we have very simple blocks. I need to determine what the
next step is. How can I implement declarations? For now, let's assume
that all declared values live on the stack. How do we determine how
much stack are we going to use for a given function? Do we count the
number of declarations? Maybe when parsing a top-level compound
statement we keep track of the subtrees in the parse tree that require
stack space. We add them up recursively and when we reach the top
level compound statement we know exactly how much stack we need.

But first, I need to implement declarations. Right now we have simple
functions that return ints. I feel like keeping with the same
philosophy and implementing very simple declarations. Maybe just ints
on the stack.

-- Before

What happens if we run out of registers?
What is register spilling?

You get the current scope, and you add up the size that you need to
cover all the stack variables. Then you take into account all the
registers that spill onto the stack.

Block objects keep track of their children and adjust their stack
requirements based on them.

Register spills are handled by a spill area within the block object
stack space.
