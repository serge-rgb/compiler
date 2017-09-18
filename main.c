
int
main(void) {
   Arena a = {0};
   stringInit(&a);
   char* file_name = "test.c";

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
         codegen.symbol_table = &p.symbol_table;
         codegenInit(&codegen);
         codegenTranslationUnit(&codegen, tree);
      }
      deallocate(&tmp_parser_arena);
      printf("FileStream says it's on line %" FORMAT_U64 "\n", file_stream.line_number);
      fileStreamClose(&file_stream);
      htmlEnd(&html);
   }

   codegenFinish();

   return result;
}

#undef NUM_BYTES
