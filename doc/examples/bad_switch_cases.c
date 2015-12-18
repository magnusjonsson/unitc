#define unit(u) __attribute__((unit(u)))

int main(int argc, char **argv) {
  switch(0) {
  case 0 .. 1:
    1 + (int unit(m)) 2;
  }
  return 0;
};
