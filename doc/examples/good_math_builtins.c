#include <math.h>

#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  double x unit(m/s);
  double y unit(m/s);
  double z unit(m/s);
  z = fabs(x);
  z = fmin(x,y);
  z = fmax(x,y);
  return 0;
}
