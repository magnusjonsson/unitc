#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  double v unit(m/s);
  double t unit(s);
  int a unit(m/s/s) = v / t;
  return 0;
}
