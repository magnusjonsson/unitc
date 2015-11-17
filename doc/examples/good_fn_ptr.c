int f(int x, int y) {
  return x + y;
}
int main(int argc, char **argv) {
  int (*g)(int x, int y) = &f;
  int (*h)(int x, int y) = f;
  int (*p)(int x, int y) = 1 ? f : &f;
  return (*g)(1, 2) + g(1,2) + h(1,2) + p(1,2) + (*f)(1,2) + (&f)(1,2);
}
