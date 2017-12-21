void addAll(file [] a)
{
  string line;

  while(!isFileEnd(a[0]))
  {
    line = readFile(a[0],200);
    printstring(line);
  }
  close(a[0],line);
}

int main()
{

  file [1] myfiles;
  int [3] x;

  myfiles[0] = open("text.txt", "r");


  x[0] = 5;

  addAll(myfiles);


    return 0;
}
