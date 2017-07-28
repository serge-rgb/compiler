
int
main(void) {
   Arena a = {0};
   codegenInit();
   stringInit(&a);
   FILE* fd = fopen("test.c", "r");

   int result = 0;

   if (fd) {
      FileStream file_stream = {0};
      if (!fileStreamInit(&file_stream, "test.c")) {
         fprintf(stderr, "ERROR: Could not read file.\n");
         result = 1;
      }
      else {
         Token* tokens = tokenize(&a, &file_stream);

         Parser p = {0};
         Arena tmp_parser_arena = {0};
         p.arena = &tmp_parser_arena;
         p.token = tokens;

         Codegen codegen = {0};
         codegen.arena = &tmp_parser_arena;

         AstNode* tree = parseTranslationUnit(&p);
         if (tree) {
            codegenEmit(&codegen, tree);
         }
         deallocate(&tmp_parser_arena);
      }

   }

   codegenFinish();

   Html html = {0};
   htmlBegin(&html);
   htmlEmit(&html, "shown", "hidden");
   htmlEnd(&html);

   return result;
}

#undef NUM_BYTES
