
int main()
{
  int [2] a;
  file [2] s;
  string line;
  file myfile;

  string p;
  p = "Hello";

  myfile = open("text.txt", "r");
  s[1] = myfile;

  while(!isFileEnd(s[1]))
  {
    line = readFile(s[1],200);
    printstring(line);
  }
  close(s[1],line);

  a[0] = 1;

  print(a[0]);
  return 0;
}
