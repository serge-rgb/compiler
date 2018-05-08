
int main() {
   int r = 0;
   for (         ; r != 50; r += 1) ;
   for (int r = 0; r != 50; r += 1) {}

   return r == 50;
}
