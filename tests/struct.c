
int
main() {
   struct foo {
      int x;
      int y;
      int z;
   };
   struct foo myvar;
   struct foo myothervar = myvar;
   return myothervar.y;
}
