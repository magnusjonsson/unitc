#include <math.h>

#define unit(u) __attribute__((unit(u)))

unit(m) float f(unit(m*m) float x) {
  return sqrt(x);
}

unit(sqrt(m)) float g(unit(m) float x) {
  return sqrt(x);
}

unit(1) float h(void) {
  return sqrt(0);
}
