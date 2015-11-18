struct x {
  int a;
  struct y {
    int b;
  } c;
  int d;
} e;

int main(int argc, char **argv) {
  return e.a + e.c.b + e.d;
}
