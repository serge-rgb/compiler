char
myFunction(int p) {
   return p;
}

int
main() {
   char foo = 3;
   int my_pretty_variable = 42;

   if (foo >= 3)
      return myFunction();
   return 0;
}
