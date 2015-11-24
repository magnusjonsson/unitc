int main(int argc, char **argv) {
  char x[5];
  char *y = x;
  void *z = x;
  return x[0] + x[2] + *x;
}
