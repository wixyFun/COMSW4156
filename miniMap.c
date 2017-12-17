#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>



void miniMap(FILE * inputFile, int (*func_ptr)(int,int))
{
   printf("%d\n", 100);

   func_ptr(2,3);

}
