
int
main() {
   struct foo {
      int x;
      int y;
   };
   struct foo myvar;
   myvar.x = 1;
   myvar.y = 2;
   return myvar.x;
}
