char
myFunction(int p) {
   return 41;
}

int
main() {
   char foo = 3;
   int my_pretty_variable = 42;

   if (foo > 3)
      return myFunction(1);
   else
      return foo + my_pretty_variable;
   return 0;
}
