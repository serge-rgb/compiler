struct veci {
   int x;
   int y;
   int z;
};

int procStruct(struct veci foo) {
   return 10*foo.x + foo.y;
}

int main() {
   struct veci foo;
   foo.x = 4;
   foo.y = 2;
   foo.z = 1;
   return procStruct(foo) == 42;
}
