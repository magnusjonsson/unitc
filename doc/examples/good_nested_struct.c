struct x {
  struct y {
    int a;
  } b;
} c;

int main(int argc, char **argv) {
  return c.b.a;
}
