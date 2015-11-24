int main(int argc, char **argv) {
  int i = 0;
 label:
  if (i == 0) goto done;
  i++;
  goto label;
 done:
  return;
}
