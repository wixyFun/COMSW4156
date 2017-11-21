#include<stdio.h>
#include<string.h>

/*
Splitting large file into smaller files by file size
https://stackoverflow.com/questions/30222271/split-file-into-n-smaller-files-in-c
*/

// #define SEGMENT long 10 //approximate target size of small file

size_t file_size(char *name);//function definition below

void splitfile(char inputFileName[])
{
  double segment = 1000.0; //approximate target size of small file
  //int sizeFile_int = 1000000;
  double segments=0;
  int i, len, accum;
  FILE *fp1, *fp2;
  //  size_t sizeFile = file_size((char *)inputFileName);
  size_t sizeFile = file_size(inputFileName);
  printf("sizeFile is %lu\n", sizeFile);
  printf("segment is %f\n", segment);

  printf("file_size / segment is: %f\n", sizeFile / segment);
  printf("file_size / segment is: %i\n", sizeFile / segment);
  //  printf("file_size_int / segment is: %i\n", sizeFile_int / segment);

  segments = sizeFile/segment + 1;//ensure end of file
  printf("segments is %i\n", segments);
  char filename[260]={"smallFileName_"};//base name for small files.
  char largeFileName[]= {"bigbang.txt"};//change to your path
  char smallFileName[260];
  char line[1080];

  fp1 = fopen(largeFileName, "r");
  if(fp1)
  {
    for(i=0;i<segments;i++)
    {
      accum = 0;
      sprintf(smallFileName, "%s%d.txt", filename, i);
      fp2 = fopen(smallFileName, "w");
      if(fp2)
      {
        while(fgets(line, 1080, fp1) && accum <= segment)
        {
          accum += strlen(line);//track size of growing file
          fputs(line, fp2);
        }
        fclose(fp2);
      }
    }
    fclose(fp1);
  }
}


size_t file_size(char* name)
{
  printf("%s\n", name);

  int i=0;
  char edited_name[100];

  while(name[i+2]!='\0')
  {
      edited_name[i] = name[i+1];             
      i++;
  }
  edited_name[i]='\0';
  printf("%s\n",edited_name);

  // const char* fn = name;
  char mode[] = "rb";
  FILE *fp = fopen(edited_name, mode); //must be binary read to get bytes

  size_t length = -1;
  if (fp)
  {
    size_t pos = ftell(fp) ;
    fseek(fp, 0, SEEK_END);
    length = ftell(fp);
    fclose(fp);
  }
  else
  {
    printf("Couldn't open file - name: %s, mode: %s\n", edited_name, mode);
  }
  printf("This is length %i\n", length);

  return length;


}
