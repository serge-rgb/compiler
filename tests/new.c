int bar(int x) {
   return x;
}

struct Foo {
   int i;
   int j;
   int k;
   int l;
};

int main() {
   struct Foo foo;
   foo.i = 1;
   struct Foo bar;
   bar = foo;
   return bar.i;
}
