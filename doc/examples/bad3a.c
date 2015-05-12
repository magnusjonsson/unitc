#define unit(u) __attribute__((unit(u)))

extern double atof(const char *str);
extern int printf(const char *format, ...);

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("Usage: example3 <velocity> <time>\n");
    return 1;
  }

  double v unit(m/s) = (double unit(m)) atof(argv[1]);
  double t unit(s) = (double unit(s)) atof(argv[2]);
  double a unit(m/s/s) = v / t;

  printf("Velocity: %f\n", v);
  printf("Time: %f\n", t);
  printf("Acceleration: %f\n", a);

  return 0;
}
