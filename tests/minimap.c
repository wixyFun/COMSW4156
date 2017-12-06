

void * minimap(FILE* inputFile, void (*split_ptr)(file), void* (*map_ptr)(file), void (*reduce_ptr)(void *))
{
  File *splitedFiles = split_ptr(inputFile);


}
