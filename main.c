ErrorCode
compileTranslationUnit(char* file_name, char* outfile) {
   fprintf(stderr, "Compiling file %s\n", file_name);

   Arena a = {0};
   ErrorCode result = Ok;

   FileStream file_stream = {0};
   if (!fileStreamInit(&file_stream, file_name)) {
      fprintf(stderr, "ERROR: Could not read file.\n");
      result = CouldNotReadFile;
   }
   else {
      Token* tokens = tokenize(&a, &file_stream);

      Parser p = {0};
      initParser(&p);
      Arena tmp_parser_arena = {0};
      p.arena = &tmp_parser_arena;
      p.token = tokens;
      p.file_name = file_name;

      Codegen codegen = {0};
      codegen.file_name = file_name;
      codegen.arena = &tmp_parser_arena;

      AstNode* tree = parseTranslationUnit(&p);
      if (!tree) {
         parseError(&p, "Could not parse file.");
      }
      else {
         codegenInit(&codegen, outfile, PlatformDefaultTarget);
         codegenTranslationUnit(&codegen, tree);
      }
      deallocate(&tmp_parser_arena);
      fileStreamClose(&file_stream);
   }

   return result;
}

ErrorCode
parseArgument(void** out, char** argv, int i, int argc) {
   ErrorCode result = Fail;

   char* arg = argv[i];
   if (arg[0] == '-') {
      if (strlen(arg) != 2) {
         result = InvalidArgument;
         SccErrorMessage = "SCC flags must be single-character";
      }
      else {
         switch (arg[1]) {
            case 'a': {
               result = Flag;
               (*out) = (void*)CompilerFlag_TEST_ALL;
            } break;
            default: {
               result = InvalidArgument;
               SccErrorMessage = "Unrecognized flag";
            } break;
         }
      }
   }
   else {
      result = Filename;
      (*out) = arg;
   }
   return result;
}

void
printHelp() {
   printf("Usage: scc {filename}+\n");
}

b32
filterCFiles(char* n) {
   b32 result = false;
   i64 len = strlen(n);
   if (len > 3 &&
       n[len-1] == 'c' &&
       n[len-2] == '.')  {
      result = true;
   }
   return result;
}

ErrorCode
processSingleFile(char* file) {
   ErrorCode err = Ok;
   char outfile[PathMax] = Zero; {
      int written = snprintf(outfile, PathMax, "%s", file);
      for (int i = written-1;
           i >= 0;
           --i) {
         if (outfile[i] == '.') {
            outfile[i] = '\0';
            break;
         }
      }
   }
   if (Ok != (err = compileTranslationUnit(file, outfile))) {
      fprintf(stderr, "Could not compile file %s\n", file);
   }
   else {
      err = platformCompileAndLinkAsmFile(outfile);
   }
   return err;
}


int /*ErrorCode*/
main(int argc, char** argv) {
   Arena temp_arena = {0};

   ErrorCode result = Ok;

   stringInit(&(Arena){0});

   char** s_files = 0;
   CompilerFlag* compiler_flags = NULL;

   if (argc < 2) {
      printHelp();
      return WrongNumberOfArguments;
   }

   for (int i = 1; i < argc; ++i) {
      printf("Arg %d, [ %s ]\n", i, argv[i]);
      void* out = 0;
      switch (parseArgument(&out, argv, i, argc)) {
         case Filename: {
            bufPush(s_files, argv[i]);
         } break;
         case Flag: {
            CompilerFlag flag = (CompilerFlag)out;
            bufPush(compiler_flags, flag);
         } break;
         case InvalidArgument: {
            fprintf(stderr, "Error when processing arguments: %s\n", SccErrorMessage);
         } break;
         default: {
            fprintf(stderr, "Unhandled error during argument parsing.\n");
         }
      }
   }

   enum {
      Action_COMPILE_FILE,
      Action_TEST_ALL,

   } action = Action_COMPILE_FILE;

   for (sz i = 0; i < bufCount(compiler_flags); ++i) {
      if (compiler_flags[i] == CompilerFlag_TEST_ALL) {
         action = Action_TEST_ALL;
      }
   }

   switch (action) {
      case Action_TEST_ALL: {
         if (bufCount(s_files)) {
            fprintf(stderr, "Invalid file name input when testing compiler.\n");
            exit(Fail);
         }
         char** test_files = NULL;
         char test_dir[PathMax] = "tests";
         platformPathAtBinary(test_dir, ArrayCount(test_dir));
         platformListDirectory(&test_files, test_dir, filterCFiles);
         for (sz i = 0; i < bufCount(test_files); ++i) {
            printf("Test file %s\n", test_files[i]);
            processSingleFile(test_files[i]);

            char* trimmed = cloneString(&temp_arena, test_files[i]);
            size_t len = strlen(trimmed);
            for (i64 i = len-1; i >= 1; --i) {
               if (trimmed[i] == '.') {
                  trimmed[i] = '\0';
                  break;
               }
            }

            char* exe = platformOutputBinaryFilename(&temp_arena, trimmed);

             if (platformRunProcess(&exe, 1, 1) != Ok) {
               fprintf(stderr, "Test failed\n");
               //Break;
               // Run it again, probably under a debugger.
               processSingleFile(test_files[i]);
               break;
            }
         }
         bufFree(test_files);
      } break;
   }

   bufFree(compiler_flags);

   if (bufCount(s_files) > 1) {
      NotImplemented("More than one file.");
   }

   for (sz i = 0 ; i < bufCount(s_files); ++i) {
      result = processSingleFile(s_files[i]);
   }

   deallocate(&temp_arena);
   return result;
}


#undef NUM_BYTES
