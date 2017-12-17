/*
* Files I/O library
*
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>


void* open(void* filename1, char* mode)
{
   char *filename = (char *)filename1;
   FILE *mfile = fopen (filename , mode);

  //  int result = addjamie(1,3);
  //  printf("THis is printjamie: %i\n", result);
  //  split(mfile);

   return (void *)mfile;
}

void* readFile(void * fp_void, int size_buf)
 {
   FILE * fp = (FILE *) fp_void;
   char *buffer = malloc(size_buf+1);
   if ( fp != NULL )
   {
      fgets (buffer, sizeof buffer, fp);
   }
   else
   {
      return NULL;
   }
   return (void *) buffer;
 }

bool isFileEnd (void * fp_void)
{
  FILE * fp = (FILE *) fp_void;
   if (!feof(fp))
     return false;
   else
      return true;
}

//free memory from file pointer and the buffer used
 void close(void * fp_void, void * buffer)
 {
   FILE * fp = (FILE *) fp_void;
   free(buffer);
   fclose(fp);
 }
 

#ifdef BUILD_TEST
int main(void)
{
void * temp = open("text.txt", "r");
 FILE * fp = (FILE *) temp;

 char * buf;

 while(! isFileEnd(fp))
 {
   buf = readFile(fp, 200);
   printf("%s\n", buf );
 }


 close(fp,buf);
}
#endif
