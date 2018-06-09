
struct veci {
   int x;
   int y;
   int z;
};


int proc_struct(struct veci foo) {
   return foo.y;
}

int main() {
   struct veci foo;
   foo.x = 4;
   return proc_struct(foo);
}
