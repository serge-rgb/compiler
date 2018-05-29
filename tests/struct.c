
int
main() {
   struct foo {
      int x;
      int y;
   };
   struct foo myvar;
   myvar.x = 2;
   myvar.y = 1;
   struct foo myothervar = myvar;
   return myothervar.y;
}
