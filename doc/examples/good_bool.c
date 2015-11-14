#include <stdbool.h>
int main(int argc, char **argv) {
  bool a = true;
  bool b = false;
  bool c = a & b;
  bool d = a | b;
  bool e = a ^ b;
  bool f = a && b;
  bool g = a || b;
  return 0;
}
