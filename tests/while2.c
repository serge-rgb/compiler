float testFunc(float x, int pow) {
   while (pow) {
      x *= 2;
      pow--;
   }
   return x;
}

int main() {
   return testFunc(1, 10) == 1024;
}