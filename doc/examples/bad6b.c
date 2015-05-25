#define unit(u) __attribute__((unit(u)))

struct vars {
  double v unit(m/s);
  double t unit(s);
  double a unit(m/s/s);
};

int main(int argc, char **argv) {
  struct vars x;
  x.a = x.w / x.t;
  return 0;
}
