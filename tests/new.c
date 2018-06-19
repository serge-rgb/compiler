int main() {
   int foo=1;
   if (foo)
      foo += 3;
   else
      foo += 3;
   for (int i = 0; i < 42; i += 1) {
      foo += 1;
   }
   return foo;
}
