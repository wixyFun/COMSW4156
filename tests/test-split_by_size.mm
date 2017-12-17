

int main()
{
  file myfile;
  file outputfilelist;
  string test;

  myfile = open("bigbang.txt", "r");
  split_by_size(myfile, 500);

  /*outputfilelist = split_by_quant(myfile, 10);*/

  test = "hello world";
  printstring(test);

  return 0;
}
