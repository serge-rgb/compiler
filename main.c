int
compileTranslationUnit(char* file_name, char* outfile) {
   fprintf(stderr, "Compiling file %s\n", file_name);

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
      codegen.config |= PlatformDefaultTarget;

      AstNode* tree = parseTranslationUnit(&p);
      if (!tree) {
         parseError(&p, "Could not parse file.");
      }
      else {
         codegenInit(&codegen, outfile);
         codegenTranslationUnit(&codegen, tree);
         codegenFinish();
      }
      deallocate(&tmp_parser_arena);
      fileStreamClose(&file_stream);
      htmlEnd(&html);
   }

   return result;
}

int
main(int args_n, char** args) {
   u32* foo = malloc( 4 * 10 );

   foo[10] = 1;

   return 0;
}

#if 0
static void
printargs(char** args, int nargs) {
   for (int i = 0; i < nargs; ++i) {
      printf("%s ", args[i]);
   }
   printf("\n");
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
         // TODO: Redirect stderr
         int res = compileTranslationUnit(file_name, outfile);
         if (res == 0) {
            // Call nasm from here.
            char asm_file[PathMax] = {0};
            snprintf(asm_file, PathMax, "%s.asm", outfile);
            char obj_file[PathMax] = {0};
            snprintf(obj_file, PathMax, "%s.o", outfile);
            pid_t pid = fork();
            printf("Running nasm\n");
            int nasm_status = 0;
            if (pid == 0) {
               char* nasm_args[] = { "nasm", "-f", "macho64", asm_file };
               printargs(nasm_args, ArrayCount(nasm_args));
               execve("/usr/local/bin/nasm", nasm_args, NULL);
            }
            else if (pid == wait(&nasm_status)) {
               if (WIFEXITED(nasm_status) && WEXITSTATUS(nasm_status) == 0) {
                  printf("Running ld\n");
                  char* ld_args[] = { "ld", "-arch", "x86_64", "-e", "_start", obj_file, "/usr/lib/libSystem.dylib", "-o", outfile };
                  pid = fork();
                  if (pid == 0) {
                     printargs(ld_args, ArrayCount(ld_args));
                     execve("/usr/bin/ld", ld_args, NULL);
                  }
                  else if (pid == wait(NULL)) {
                     printf("Running %s\n", outfile);
                     char* out_args[] = { outfile };
                     if (fork() == 0) {
                        execve(outfile, out_args, NULL);
                     }
                     int status = 0;
                     wait(&status);
                     if (WIFEXITED(status)) {
                        printf("Returned status: %d\n", WEXITSTATUS(status));
                        if (WEXITSTATUS(status) != 1) {
                           printf("ERROR: test failed.\n");
                           exit(1);
                        }
                     }
                     else {
                        printf("Program exited incorrectly.");
                     }
                  }
               } else {
                  fprintf(stderr, "nasm failed\n");
               }
            }
         } else {
            printf("Compilation error.");
            exit(1);
         }
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
                     /* {"tests/basic.c", "tests/out.basic"}, */
                     /* {"tests/test.c", "tests/out.test"}, */
                     /* {"tests/for_1.c", "tests/out.for_1"}, */
                     /* {"tests/for_2.c", "tests/out.for_2"}, */
                     /* {"tests/param_1.c", "tests/out.param_1"}, */
                     /* {"tests/param_2.c", "tests/out.param_2"}, */
                     /* {"tests/while1.c", "tests/out.while1"}, */
                     /* {"tests/comment.c", "tests/out.comment"}, */
                     {"tests/struct.c", "tests/out.struct"},
                  };

                  for (int test_i = 0; test_i < ArrayCount(tests); ++test_i) {
                     pid_t pid = fork();
                     if (/*child process*/pid == 0) {
                        char* child_args[] = { "compiler", "-o", tests[test_i].out, tests[test_i].fname };
                        pid = fork();
                        if (pid == 0) {
                           execve("./compiler", child_args, NULL);
                        }
                        else if (pid == wait(NULL)) {
                           int status = 0;
                           wait(&status);
                           if (!(WIFEXITED(status) && 1 != WEXITSTATUS(status))) {
                              printf("Test failed: %s\n", tests[test_i].fname);
                              break;
                           }
                        }
                     }
                     else if (pid == wait(NULL)){
                        printf ("Finished running ./compiler -o %s %s\n", tests[test_i].out, tests[test_i].fname);
                     }
                  }
               }
            } else {
               fprintf(stderr, "Expected test argument..\n");
            }
         }
      }
   }
}
#endif

#undef NUM_BYTES
