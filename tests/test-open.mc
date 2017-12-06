int addjamie(int a, int b)
{
  return a+b;
}

int main()
{
  file myfile;
  string line;

  myfile = open("text.txt", "r");

  while(!isFileEnd(myfile))
  {
    line = readFile(myfile,200);
    printstring(line);
  }
  close(myfile,line);

}
