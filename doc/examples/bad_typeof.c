#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  double v unit(m/s);
  double t unit(s);
  typeof (v/t) i = v / t;
  int b = i == v;
  return 0;
}
