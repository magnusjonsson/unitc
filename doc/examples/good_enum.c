enum yesno {
  yes,
  no = yes + 1,
};

enum yesno decision;

enum /* anonymous */ {
  oui,
  non,
};

int x = oui;
int y = non;

struct s {
  enum nested_named { NESTED_NAMED } f;
  enum { NESTED_ANONYMOUS } g;
};

int z = NESTED_NAMED + NESTED_ANONYMOUS;
