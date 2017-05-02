#if 0

# This file builds and runs itself.

clang -g -Wall -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler && ./compiler

#endif

#include <stddef.h>
#include <stdint.h>

#include "std.h"
#include "common.h"

#include "memory.c"
#include "string.c"
#include "lexer.c"
#include "codegen.c"
#include "parse.c"
#include "main.c"
