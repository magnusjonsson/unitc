int main(int argc, char **argv) {
  char *x, *y;
  char z[5];
  x += 0;
  x -= 0;
  x += 1;
  x -= 1;
  z + 1;
  z - 1;
  1 + z;
  int w = x - y;
  _Bool b;
  b = x == y;
  b = x > y;
  b = x < y;
  b = x >= y;
  b = x <= y;
  b = !x;
  b = x && y;
  b = x && z;
  b = x || y;
  b = !x;

  // void pointer arithmetic, disallwed by the standard, allowed by gcc
  void *v;
  v = v + 1;
  v = 1 + v;
  v = v - 1;

  return 0;
}
