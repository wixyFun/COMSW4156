/*
* Files I/O library
*
*/
#include <stdio.h>

void* open(void* filename1)
{

   //static const char filename[] = "text.txt";
   char *filename = (char *)filename1;
   FILE *mfile = fopen ( filename, "r" );

   printf("hello\n");

  return (void *)mfile;

}



#ifdef BUILD_TEST
int main(void)
{
  open("text.txt");
}
#endif
