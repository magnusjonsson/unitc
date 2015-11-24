int main(int argc, char **argv) {
  int a = sizeof(a);
  int b = sizeof(c), c = sizeof(b);
  return a + b + c;
}
