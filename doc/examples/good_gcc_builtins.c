int main(int argc, char **argv) {
  __builtin_va_list va_list;
  int a;
  a = __builtin_bswap32(a);
  a = __builtin_bswap64(a);
  a = __builtin_constant_p(1);
  a = __builtin_constant_p("string");
  a = __builtin_strchr("string", 's');
  return a;
}
