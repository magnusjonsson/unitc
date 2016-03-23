int main(int arg, char **argv) {
  return __alignof(int) + __alignof arg;
}
