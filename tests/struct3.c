
struct veci {
   int x;
   int y;
   int z;
};


int proc_struct(struct veci* foo) {
   return foo.y;
}

int main() {
   struct veci foo;
   foo.x = 4;
   foo.y = 2;
   foo.z = 1;

   struct veci *pfoo = &foo;

   return proc_struct(pfoo) == 2;
}
