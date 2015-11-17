int f(int x, int y) {
  return x + y;
}
int main(int argc, char **argv) {
  int (*g)(int x, int y) = &f;
  int (**h)(int x, int y) = g;
  return 0;
}
