struct point { int x; int y; };

void varargsfn(int lastarg, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, lastarg);
  __builtin_va_end(ap);
}
int main(int argc, char **argv) {
  int a;
  a = __builtin_bswap32(a);
  a = __builtin_bswap64(a);
  a = __builtin_constant_p(1);
  a = __builtin_constant_p("string");
  char *b = __builtin_strchr("string", 's');
  a = __builtin_expect(1, a);
  a = __builtin_strlen("string");
  a = __builtin_strcmp("string1", "string2");
  a = __builtin_offsetof(struct point, y);
  char c[100];
  b = __builtin_strcpy(c, "a");
  b = __builtin_strncpy(c, "a", 1);
  a = __builtin_ctzl(a);
  varargsfn(0);
  return a;
}
