char
myFunction(int p, int q, int r, int s) {
   return 42;
}

int
main() {
   char foo = 3;
   int my_pretty_variable = 42;

   if (foo > 3)
      return myFunction();
   else
      return foo + my_pretty_variable;
   return 0;
}
