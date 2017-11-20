#include<stdio.h>
#include<string.h>

/*
Splitting large file into smaller files by file size
https://stackoverflow.com/questions/30222271/split-file-into-n-smaller-files-in-c
*/

// #define SEGMENT long 10 //approximate target size of small file

long file_size(char *name);//function definition below

void splitfile(char inputFileName[])
{
   int segment = 5000; //approximate target size of small file
   int segments=0, i, len, accum;
   FILE *fp1, *fp2;
   long sizeFile = file_size((char *)inputFileName);
   printf("sizeFile is %lu\n", sizeFile);
   printf("segment is %i\n", segment);
   long test1 = sizeFile / 5000;

   printf("test for lu / int: %llu\n", test1);
   printf("test for long/long: %llu\n", 18446744073709551615 / segment);
   printf("test for long/int returning long: %ld\n", sizeFile / 10 );
   printf("test for long/float returning float: %f\n", sizeFile / 10.0 );

   printf("This should be segments -1 : %lu\n", sizeFile/segment);
   segments = sizeFile/segment + 1;//ensure end of file
   printf("segment size is %llu", segments);
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


long file_size(char *name)
{
    FILE *fp = fopen(name, "rb"); //must be binary read to get bytes

    long size=-1;
    if(fp)
    {
        fseek (fp, 0, SEEK_END);
        size = ftell(fp)+1;
        fclose(fp);
    }
    return size;
}



  // void splitfile(char fileName[])
  // {
  //   FILE *ptr_file;
	// 	int x;
  //
	// 	ptr_file =fopen(fileName, "w");
  //
	// 	if (!ptr_file)
  //     printf("Error in splitfile\n");
  //
	// 	for (x=1; x<=10; x++)
  //   {
  //     printf("%d\n",x);
	// 		fprintf(ptr_file,"%d\n", x);
  //   }
  //
	// 	fclose(ptr_file);
  //
  // }
