int printAdd(int a, int b)
{
    int sum;
    sum = a + b;
    print(sum);
    return sum;
}



int main()
{
  file myfile;
  string line;



  myfile = open("text.txt", "r");

  miniMap(myfile,printAdd);



  while(!isFileEnd(myfile))
  {
    line = readFile(myfile,200);
  }
  close(myfile,line);

}
