#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  ((unit(foo) float) 1.0f) + 1.0f;
  return 0;
}
