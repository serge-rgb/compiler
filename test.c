
int
main() {
   int foo = 101;
   if (foo < 100) {
      if (foo > 1) {
         if ( foo == 42 ) {
            return foo;
         }
      }
   }
   else if (foo)
      return 2;

   return 0;
}
