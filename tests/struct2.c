
struct veci {
   int x;
   int y;
};


int proc_struct(struct veci foo) {
   return foo.y;
}

int main() {
   struct veci foo;
   foo.x = 4;
   foo.y = 1;
   return proc_struct(foo);
}
