#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
 bad:
  1 + (int unit(m)) 2;
  return 0;
};
