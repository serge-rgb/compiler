int
compileTranslationUnit(char* file_name, char* outfile) {
   fprintf(stderr, "Compiling file %s\n", file_name);

   Arena a = {0};
   stringInit(&a);

   int result = 0;

   FileStream file_stream = {0};
   if (!fileStreamInit(&file_stream, file_name)) {
      fprintf(stderr, "ERROR: Could not read file.\n");
      result = 1;
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
         machFinish();
      }
      deallocate(&tmp_parser_arena);
      fileStreamClose(&file_stream);
   }

   return result;
}

enum {
   Arg_FILENAME,
   Arg_NOTHING,
};
int
parseArgument(char* txt) {
   if (txt[0] == '-') {
      NotImplemented("Compiler opts");
   }
   else {
      return Arg_FILENAME;
   }
   return Arg_NOTHING;
}

void
printHelp() {
   printf("Usage: scc {filename}+");
}

int
main(int argc, char** argv) {
   enum {
      Ok = 0,
      CouldNotCompile = 1,
   } result = Ok;

   char** files = 0;

   if (argc < 2) {
      printHelp();
      return 42;
   }

   for (int i = 1; i < argc; ++i) {
      printf("Arg %d, [ %s ]\n", i, argv[i]);
      switch (parseArgument(argv[i])) {
         case Arg_FILENAME: {
            bufPush(files, argv[i]);
         } break;
      }
   }

   if (bufCount(files) > 1) {
      NotImplemented("More than one file.");
   }

   for (sz i = 0 ; i < bufCount(files); ++i) {
      char outfile[PathMax] = Zero; {
         int written = snprintf(outfile, PathMax, "%s", files[i]);
         for (int i = written-1;
              i >= 0;
              --i) {
            if (outfile[i] == '.') {
               outfile[i] = '\0';
               break;
            }
         }
      }
      if (0 != compileTranslationUnit(files[i], outfile)) {
         fprintf(stderr, "Could not compile file %s\n", files[i]);
         result = CouldNotCompile;
      }
      else {
         result = platformCompileAndLinkAsmFile(outfile);
      }

   }
   return result;
}


#undef NUM_BYTES
