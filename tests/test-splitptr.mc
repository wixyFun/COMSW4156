


int main()
{
  file myfile;
  file outputfilelist;
  string haystack;
  string needle;
  string ret;
  file tmpfile;
  string line;

  myfile = open("bigbang.txt", "r");
  /*split_by_size(myfile, 500);*/

  outputfilelist = split_by_quant(myfile, 10);

  tmpfile = outputfilelist;

  while(!isFileEnd(tmpfile))
  {
    line = readFile(tmpfile,200);
    printstring(line);
    printstring("hello");
  }

  haystack = "TutorialsPoint";
  needle = "Point";
  ret = strstr(haystack, needle);

  printstring(ret);

  return 0;
}
