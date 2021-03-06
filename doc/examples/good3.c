#define unit(u) __attribute__((unit(u)))

extern double atof(const char *str);
extern int printf(const char *format, ...);

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("Usage: %s <velocity> <time>\n", argv[0]);
    return 1;
  }

  double v unit(m/s) = (double unit(m/s)) atof(argv[1]);
  double t unit(s) = (double unit(s)) atof(argv[2]);
  double a unit(m/s/s) = v / t;

  printf("Velocity: %f m/s\n", v);
  printf("Time: %f s\n", t);
  printf("Acceleration: %f m/s^2\n", a);

  return 0;
}
