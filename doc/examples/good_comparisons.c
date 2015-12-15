#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  int a;
  double unit(m) x = 0;

  a = x == x;
  a = x != x;
  a = x > x;
  a = x >= x;
  a = x < x;
  a = x <= x;

  a = x == 0;
  a = x != 0;
  a = x > 0;
  a = x >= 0;
  a = x < 0;
  a = x <= 0;

  a = 0 == x;
  a = 0 != x;
  a = 0 > x;
  a = 0 >= x;
  a = 0 < x;
  a = 0 <= x;

  a = 0 == 0;
  a = 0 != 0;
  a = 0 > 0;
  a = 0 >= 0;
  a = 0 < 0;
  a = 0 <= 0;

  a = !x;

  return 0;
}
