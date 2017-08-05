int
myFunction() {
   return 0;
}

int
main() {
   int my_pretty_variable = 42;
   int another_var = 1;
   int yup = 2;

   myFunction();

   if (1) {
      return 3;
   }

   if (my_pretty_variable > 41) {
      if (my_pretty_variable > 1) {
         return 0;
      }
      return 1;
   }
   return 1;
}
