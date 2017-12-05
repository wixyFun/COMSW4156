void* map(file chunk)
{

}

/*Reducer: user defined function that aggregates all the return values after map function is applied reduce(void* result); return void* total*/
void reduce(void* result)
{

}

file split(file inputFile)
{
  file outputfilelist;
  outputFileList = plit_by_quant(inputFile, 10);
  return outputFileList; 
}


int main()
{
  file myfile;
  myfile = open("bigbang.txt");

  void (*split_ptr)(file, int);
  split_ptr = &split;

  void* (*map_ptr)(file);
  map_ptr = &map;

  void (*reduce_ptr)(void *);
  reduce_ptr = &reduce;

  minimap(myfile, split_ptr, map_ptr, reduce_ptr);

  return 0;

}
