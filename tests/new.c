int bar(int x) {
   return x;
}

int main() {
   int foo = 0;
   for (int i = 0; i < 42; i++) {
      foo += 1;
   }
   return foo == bar(42);  // TODO: This comparison fails
}
