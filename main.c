#include <unistd.h>

int
compileTranslationUnit(char* file_name, char* outfile) {

   fprintf(stderr, "Compiling file %s\n", file_name);

   // TODO: outfile

   Arena a = {0};
   stringInit(&a);

   int result = 0;

   Html html = {0};
   htmlBegin(&html);
   FileStream file_stream = {0};
   if (!fileStreamInit(&file_stream, file_name)) {
      fprintf(stderr, "ERROR: Could not read file.\n");
      result = 1;
   }
   else {
      Token* tokens = tokenize(&a, &file_stream);

      Parser p = {0};
      Arena tmp_parser_arena = {0};
      p.arena = &tmp_parser_arena;
      p.token = tokens;
      p.file_name = file_name;

      Codegen codegen = {0};
      codegen.file_name = file_name;
      codegen.html = &html;
      codegen.arena = &tmp_parser_arena;
#if defined(__APPLE__)
   #if defined(__MACH__)
      codegen.config |= Config_TARGET_MACOS;
   #else
      #error APPLE target not mac OS
   #endif
#elif defined(__linux__)
      codegen.config |= Config_TARGET_LINUX;
#elif defined(_WIN32)
      codegen.config |= Config_TARGET_WIN;
#else
   #error Cannot determine target
#endif

      AstNode* tree = parseTranslationUnit(&p);
      if (tree) {
         codegenInit(&codegen, outfile);
         codegenTranslationUnit(&codegen, tree);
      }
      deallocate(&tmp_parser_arena);
      fileStreamClose(&file_stream);
      htmlEnd(&html);
   }

   codegenFinish();

   return result;
}


int
main(int args_n, char** args) {

   fprintf(stderr, "scc call. arguments: %d\n", args_n);
   if (args_n <= 1) {
      fprintf(stderr, "scc - v0.0.1\n");
      exit(0);
   }

   char* outfile = "out";

   for (int arg_i = 1; arg_i < args_n; ++arg_i) {
      char* arg = args[arg_i];
      size_t arg_len = strlen(arg);

      if (arg[0] != '-') {
         char* file_name = arg;
         int res = compileTranslationUnit(file_name, outfile);
         Assert(res == 0);
      }
      else if (arg_len > 1) {
         if (arg[1] == 'o') {
            if (arg_i + 1 < args_n) {
               outfile = args[++arg_i];
            } else {
               fprintf(stderr, "Expected output file name.\n");
            }
         }
         if (arg[1] == 't') {
            if (arg_i + 1 < args_n) {
               char* test = args[++arg_i];
               if (!strcmp(test, "all")) {

                  struct Pair {
                     char* fname;
                     char* out;
                  };

                  struct Pair tests [] =  {
                     {"tests/basic.c", "out.basic"},
                     {"test.c", "out.test"},
                  };

                  for (int test_i = 0; test_i < ArrayCount(tests); ++test_i) {
                     pid_t pid = fork();


                     if (/*child process*/pid == 0) {
                        printf("About to call execve...\n");

                           char* child_args[] = { "child", "-o", tests[test_i].out, tests[test_i].fname };
                           execve("./compiler", child_args, NULL);
                     }
                  }
               }
            } else {
               fprintf(stderr, "Expected test argument..\n");
            }
         }
         if (arg[1] == 'h') {
            printf("OMG I GOT THE H FLAG\n");
            if (arg_i + 1 < args_n) {
               printf("Hello message: [%s]\n", args[++arg_i]);
            }
            else {
               fprintf(stderr, "Expected hello message.\n");
            }
         }
      }
   }


}

#undef NUM_BYTES
