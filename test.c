char
myFunction(int p, int q) {
   return p + q;
}

int
main() {
   char foo = 3;
   int my_pretty_variable = 42;

   if (foo >= 3)
      return myFunction(1, 2);
   else
      return foo + my_pretty_variable;
   return 0;
}
