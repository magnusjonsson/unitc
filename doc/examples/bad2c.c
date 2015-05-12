#define unit(u) __attribute__((unit(u)))

double unit(m/s/s) acceleration (double velocity_change unit(m/s), double time unit(s)) {
  return velocity_change / time;
}

int main(int argc, char **argv) {
  double v unit(m);
  double t unit(s);
  double a unit(m/s/s) = acceleration(v, t);
  return 0;
}
