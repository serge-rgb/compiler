int main()
{
   int x = 1;
   int *y = &x;
   int **z = &y;
   return x;
   // return **z;
}
