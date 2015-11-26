#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  unit(m/s) a = 0;
  return 0;
}
