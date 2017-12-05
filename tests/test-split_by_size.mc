

int main()
{
  file myfile;
  file outputfilelist;

  myfile = open("bigbang.txt");
  /*split_by_size(myfile, 500);*/

  outputfilelist = split_by_quant(myfile, 10);

  return 0;
}
