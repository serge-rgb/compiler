char
foo(int a) {
   return 41 + a;
}

int
main() {
   int x = foo(1);

   return x==42;
}
