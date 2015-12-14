#define unit(u) __attribute__((unit(u)))

void main(int argc, char **argv) {
  int a;
  double x unit(m);

  a = x == x;
  a = x != x;
  a = x > x;
  a = x >= x;
  a = x < x;
  a = x <= x;

  a = x == 0
  a = x != 0
  a = x > 0;
  a = x >= 0;
  a = x < 0;
  a = x <= 0;

  a = 0 == x
  a = 0 != x
  a = 0 > x;
  a = 0 >= x;
  a = 0 < x;
  a = 0 <= x;

  a = 0 == 0
  a = 0 != 0
  a = 0 > 0;
  a = 0 >= 0;
  a = 0 < 0;
  a = 0 <= 0;

  return 0;
}
