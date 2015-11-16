int main(int argc, char **argv) {
  argc += 1;
  argc -= 1;
  argc *= 2;
  argc /= 2;
  argc %= 2;
  argc <<= 1;
  argc >>= 1;
  argc |= 1;
  argc &= 1;
  argc ^= 1;
  return argc;
}
