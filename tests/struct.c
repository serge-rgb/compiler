
int
main() {
   struct foo {
      int x;
   };
   struct foo myvar;
   myvar.x = 1;
   return myvar.x;
}
