void f(void (*g)(int x)) {
  g(0);
}

void h(int y) {
}

int main(int arc, char **argv) {
  f(h);
  return 0;
}
