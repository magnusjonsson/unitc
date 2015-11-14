int main(int argc, char **argv) {
  return __builtin_constant_p(1) && __builtin_constant_p("string");
}
