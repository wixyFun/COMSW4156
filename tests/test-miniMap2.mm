int printAdd(int a, int b)
{
    int sum;
    sum = a + b;
    print(sum);
    return sum;
}


int main()
{
  file inputFile;
  string line;

  inputFile = open("bigbang.txt", "r");
  split_by_size(inputFile);

  miniMap(myfile,printAdd);

  while(!isFileEnd(myfile))
  {
    line = readFile(myfile,200);
  }
  close(myfile,line);

}
