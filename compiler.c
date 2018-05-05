#if 0
# This file builds and runs itself.

if [ `uname` = "Linux" ]; then
   clang -g -Wall -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler && ./compiler && nasm -f elf64 out.asm && ld out.o
else  # Assume it's macOS
   clang -g -Wall -Wno-missing-braces -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler # && ./compiler -t all
fi

exit 0

#endif

// Configuration


#define SCC_DEBUG

// ----

#include "std.h"
#include "common.h"

#if defined(_WIN32)
   #include "platform_windows.h"
#else
   #include "platform_unix.h"

   #if defined(__APPLE__) && defined(__MACH__)
      #include "platform_macos.h"
   #elif defined(__linux__)
      #include "platform_linux.h"
   #endif
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
