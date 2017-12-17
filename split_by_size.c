#include<stdio.h>
#include<string.h>
#include<stdlib.h>

/*
Splitting large file into smaller files by file size
https://stackoverflow.com/questions/30222271/split-file-into-n-smaller-files-in-c
*/

// #define SEGMENT long 10 //approximate target size of small file

size_t file_size(FILE *inputFile);//function definition below

void* split_by_quant(FILE *inputFile, int quant)
{
  int j, len, accum;

  FILE *fp2;

  size_t sizeFile = file_size(inputFile);

  double segment = sizeFile / quant;

  char filename[260]={"smallFileName_"};//base name for small files.
  char smallFileName[260];
  char line[1080];

  FILE *outputFileList[quant];

  for (j=0; j<quant; j++)
  {
    accum = 0;
    sprintf(smallFileName, "%s%d.txt", filename, j);
    fp2 = fopen(smallFileName, "w");
    outputFileList[j] = fp2;
    if(fp2)
    {
      while(fgets(line, 1080, inputFile) && accum <= segment)
      {
        accum += strlen(line);//track size of growing file
        fputs(line, fp2);
      }
      // fclose(fp2);
    }

  }
  rewind(inputFile);

  // for (int i=0;i<quant; i++)
  // {
  //   printf("address of file is: %d\n", outputFileList[i]);
  // }
  // FILE ** res = &outputFileList;
  void * res ;
  return (void *) res;
}


FILE* split_by_size(FILE *inputFile, int size)
{
  double segment = (double)size;
  double segments = 0;
  int i, len, accum, quant;

  FILE *fp2;

  size_t sizeFile = file_size(inputFile);

  printf("sizeFile is %lu\n", sizeFile);
  // printf("segment is %f\n", segment);
  // printf("file_size / segment is: %f\n", sizeFile / segment);
  // printf("file_size / segment is: %i\n", sizeFile / segment);

  segments = sizeFile/segment ;//ensure end of file
  quant = (int) segments;
  printf("Number of chunks is %i\n", quant);

  char filename[260]={"smallFileName_"};//base name for small files.
  char smallFileName[260];
  char line[1080];

  // FILE *outputFileList[quant];

  if(inputFile)
  {
    for(i=0;i<quant;i++)
    {
      accum = 0;
      sprintf(smallFileName, "%s%d.txt", filename, i);
      fp2 = fopen(smallFileName, "w");
      if(fp2)
      {
        while(fgets(line, 1080, inputFile) && accum <= segment)
        {
          accum += strlen(line);//track size of growing file
          fputs(line, fp2);
        }
        // fclose(fp2);
      }
    }
  }
  rewind(inputFile);

  //need to change this to fit miniMap!
  FILE* outputFileList;

  return outputFileList;

}

size_t file_size(FILE* inputFile)
{
  size_t count = -1;  /* number of characters seen */

  while (inputFile)
  {
    /* character or EOF flag from input */
    int ch;
    ch = fgetc(inputFile);
    if (ch == EOF)
    {
      break;
    }
    ++count;
  }

  printf("Number of characters is %zu\n", count);
  rewind(inputFile);
  return count;
}
