#define unit(u) __attribute__((unit(u)))

extern double atof(const char *str);
extern int printf(const char *format, ...);

int main(int argc, char **argv) {
  if (argv != 3) {
    printf("Usage: example3 <velocity> <time>\n");
  }

  double v unit(m/s) = atof(argv[1]);
  double t unit(s) = atof(argv[2]);
  double a unit(m/s/s) = v / t;

  printf("Velocity: %f\n", g);
  printf("Time: %f\n", t);
  printf("Acceleration: %f\n", a);

  return 0;
}
