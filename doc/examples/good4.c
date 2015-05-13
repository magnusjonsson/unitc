#define unit(u) __attribute__((unit(u)))

typedef double time unit(s), velocity unit(m/s), acceleration unit(m/s/s);

extern double atof(const char *str);
extern int printf(const char *format, ...);

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("Usage: %s <velocity> <time>\n", argv[0]);
    return 1;
  }

  velocity v = (velocity) atof(argv[1]);
  time t = (time) atof(argv[2]);
  acceleration a = v / t;

  printf("Velocity: %f m/s\n", v);
  printf("Time: %f s\n", t);
  printf("Acceleration: %f m/s^2\n", a);

  return 0;
}
