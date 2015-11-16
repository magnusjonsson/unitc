struct point { int x; int y; };

int argc(int argc, char **argv) {
  return ((struct point) { .x = 1, .y = 2, }).y;
}
