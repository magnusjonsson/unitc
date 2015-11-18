struct point { int x; int y; char *z; };

int main(int argc, char **argv) {
  struct point p1 = { .x = 5, .y = 6, .z = "" };
  struct point p2 = { 5, 6, "" };
  return 0;
}
