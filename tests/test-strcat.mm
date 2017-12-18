int main() {
  string x;
  string y;
  string xy;
  string z;
  string m;

  x = "hello";
  y = "world";
  m = x;
  z = y;

  xy = strcat(y, z);

  printstring(xy);

  return (0);
}
