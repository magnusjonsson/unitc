int main(int argc, char **argv) {
  int *argcp = &argc;
  return **argv + *argcp;
}
