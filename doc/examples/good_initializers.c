struct point { int x; int y; char *z; char w[5]; };

int main(int argc, char **argv) {
  struct point p1 = { .x = 5, .y = 6, .z = "", .w = "foo" };
  struct point p2 = { 5, 6, "", "foo" };
  return 0;
}
