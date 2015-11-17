struct point { int x; int y; };
int main(char *argc, char **argv) {
  struct point p;
  struct point *q;
  return p.x + q->y;
};
