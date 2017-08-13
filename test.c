int
myFunction() {
   return 42;
}

int
main() {
   char foo = 3;
   int my_pretty_variable = 42;

   if (my_pretty_variable > 41) {
      if (my_pretty_variable >= 1) {
         if (foo <= 1) {
            return myFunction();
         }
      }
      else {
         return 1;
      }
   }
   return 0;
}
