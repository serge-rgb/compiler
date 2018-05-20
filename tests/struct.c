struct foo {
   char bar;
};

int main()
{
   struct foo foo;
   foo.bar = 42;
   return foo.bar == 42;
}
