
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

   int my_pretty_variable = foo.x;

   struct veci *pfoo = &foo;

   my_pretty_variable = 2 + pfoo.y;

   // return proc_struct(foo);   // TODO: Check for incompatible types.
   return proc_struct(pfoo) == 2;
}
