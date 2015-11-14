int main(int argc, char **argv) {
  void *a = argv;
  void *b = *argv;
  argv = (void *) 0;
  return 0;
}
