#if 0

# This file builds and runs itself.

if [ `uname` = "Linux" ]; then
   clang -g -Wall -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler && ./compiler && nasm -f elf64 out.asm && ld out.o
else  # Assume it's macOS
   clang -g -Wall -Wno-missing-braces -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler && ./compiler test.c&& nasm -f macho64 out.asm && ld -arch x86_64 -e _start out.o /usr/lib/libSystem.dylib -o out
fi

exit 0

#endif

#include <stddef.h>
#include <stdint.h>

#include "std.h"
#include "common.h"

#if defined(_WIN32)
#include "platform_windows.h"
#else
#include "platform_unix.h"
#endif

#include "memory.c"
#include "hashmap.c"
#include "string.c"
#include "lexer.c"
#include "html.c"
#include "tree.c"
#include "semantics.c"
#include "parse.c"
#include "codegen.c"
#include "main.c"
