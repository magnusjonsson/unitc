#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  float a = 0.0f;
  a += ((unit(foo) float) 1.0f);
  return 0;
}
