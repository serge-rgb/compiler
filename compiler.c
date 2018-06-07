#if 0
# This file builds and runs itself.

if [ `uname` = "Linux" ]; then
   clang -g -Wall -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler && ./compiler && nasm -f elf64 out.asm && ld out.o
elif [ `uname` = "MSYS_NT-10.0" ]; then
   # *** NOTE: *** As of 2018-06-02, clang-cl is still pretty buggy when debugging with Visual Studio.
   comment_for_cleanup="-Wno-unused-parameter -Wno-shadow -Wno-unused-macros -Wno-switch-enum"
   clang-cl -Z7 $comment_for_cleanup -Wno-gnu-empty-initializer -Wno-covered-switch-default -Wno-gnu-empty-struct -Wno-shorten-64-to-32 -Wno-format-nonliteral -Wno-c++-compat -Wno-sign-conversion -Wno-string-conversion -Wno-missing-variable-declarations -Wno-sign-compare -Wno-shorten-64-to-32 -Wno-missing-noreturn -Wno-comma -Wno-pointer-arith -Wall -Wno-cast-align -Wno-missing-prototypes -Wno-deprecated-declarations -Wno-missing-braces compiler.c -link User32.lib
else  # Assume it's macOS
   echo `uname`
   clang -g -Wall -Wno-missing-braces -fno-omit-frame-pointer -fsanitize=address compiler.c -o compiler # && ./compiler -t all
fi

exit 0

#endif

// Configuration


#define SCC_DEBUG

// ----

#include "std.h"
#include "common.h"

#include "error_codes.inl"

#if defined(_WIN32)
   #include "platform_windows.c"
#else
   #include "platform_unix.c"

   #if defined(__APPLE__) && defined(__MACH__)
      #include "platform_macos.c"
   #elif defined(__linux__)
      #include "platform_linux.c"
   #endif
#endif

#include "memory.c"
#include "stretchy.c"
#include "hashmap.c"
#include "string.c"
#include "lexer.c"
#include "html.c"
#include "tree.c"
#include "semantics.c"
#include "parse.c"
#include "codegen.c"
#include "main.c"
