float testFunc(float x, int pow) {
   while (pow) {
      x *= 2;
      pow--;
   }
}

int main() {
   return testFunc(2, 10);
}