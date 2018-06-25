int main() {
   int foo = 0;
   for (int i = 0; i < 4; i++) {
      foo += 1;
      foo--;
      foo++;
   }
   return foo == 4;
}
