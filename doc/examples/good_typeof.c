#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  double v unit(m/s);
  double t unit(s);
  typeof (v/t) i = v / t;
  double a unit(m/s/s) = v / t;
  int b = i == a;
  return 0;
}
