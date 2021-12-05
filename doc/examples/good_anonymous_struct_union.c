struct foo {
  union {
    int i;
    float f;
  };
};

int main(int argc, char **argv) {
  struct foo x = { .f = 5, };
  return x.i;
}
