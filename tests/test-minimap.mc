file split(file inputFile)
{
  file outputfilelist;
  outputfilelist = split_by_quant(inputFile, 10);
  return outputfilelist;
}


int main()
{
  file myfile;
  myfile = open("bigbang.txt", "r");

  return 0;

}
