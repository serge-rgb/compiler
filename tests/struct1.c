
int
main() {
   struct foo {
      int x;
      int y;
      int z;
   };
   struct foo myvar;
   myvar.x = 1;
   myvar.y = 2;
   myvar.z = 3;
   struct foo myothervar = myvar;
   return myothervar.y == 2;
}
