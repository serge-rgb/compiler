struct veci {
   int x;
   int y;
   int z;
};

// This is starting to look like something useful!
int proc_struct(struct veci foo) {
   return 10*foo.x + foo.y;
}

int main() {
   struct veci foo;
   foo.x = 4;
   foo.y = 2;
   foo.z = 1;
   return proc_struct(foo) == 42;
}
