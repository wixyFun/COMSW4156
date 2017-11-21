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

   /*
   if ( mfile != NULL )
   {
      char line [ 128 ];
      while ( fgets ( line, sizeof line, mfile ) != NULL )
      {
         fputs ( line, stdout );
      }

   }
   else
   {
      perror ( filename );
   }

   */
  /*printf("%s\n", filename );*/
  printf("hello\n");

  return (void *)mfile;

}



#ifdef BUILD_TEST
int main(void)
{
  open("text.txt");
}
#endif
