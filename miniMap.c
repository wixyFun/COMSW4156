/*#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include<string.h>
#include <time.h>


//  @Ryan DeCosmo

//TO RUN THIS:
//gcc -Wall -g  -pthread miniMap.c -o miniMap

//TO RUN IN PRODUCTION MODE (all warnings become errors)
//gcc -Wall -g -std=c99 -Werror -pthread miniMap.c -o miniMap

void map(FILE* arg ){
  time_t startTime;
  time_t endTime;
  startTime = time (NULL);
  /////////////////////////////
  arg = fopen("sample.txt","r");
  char* word = "Inside Mapper";
  printf("%s\n", word);
  //cast file pointer here like this:
  //struct summation_struct *arg_struct = (struct summation_struct*) arg;
  endTime = time (NULL);
  printf("[THREAD RETURNED] Map function took %f seconds.\n For the sum: %llu. \n\n", difftime(endTime, startTime),arg);
  pthread_exit(0);
}

void reduce(){

    char* word = "Inside Reducer";
    printf("%s\n", word);
}

///////USE PREAD and PWRITE

void miniMap(FILE** splits,void (*mapper)(),void (*reducer)(),void *mapResults) {
//add a cast from splits to file pointers
//add a cast from mapResults to whatever ---maybe change this to context
//calculate number of files by doing -> total size of array / element size

  //  int numFiles = sizeof(&splits)/sizeof(splits[0]);
    int numFiles = sizeof(splits[0])/sizeof(splits[0][0]);
    printf("numFiles: %d\n", numFiles );

    pthread_t tids[numFiles];

    for (int i = 0; i < numFiles; i++) {
      pthread_attr_t attr;
      pthread_attr_init(&attr);
      pthread_create(&tids[i], &attr, map, (FILE*)(splits[i])); //&files[i]);

      printf("SPLITS @ %d %d\n",i,splits[i] );
    }

    // Wait until thread is done its work
    for (int i = 0; i < numFiles; i++) {
      pthread_join(tids[i], NULL);
      printf("Sum for thread %d is %d\n",
          i, i);
    }
}

int main(int argc, char **argv)
{
  //FILE** fp;
  int numFiles = 4;
  FILE **files = malloc(numFiles * sizeof(FILE *));

  int nums[5] = {1000, 2, 3, 7, 50};
  int numFi = sizeof(nums)/sizeof(nums[0]);
  printf("%i\n", numFi );

  miniMap(files,map,reduce,nums);


}


size_t file_size(FILE* inputFile)
{
  size_t count = -1;  // number of characters seen

  while (inputFile)
  {
    // character or EOF flag from input
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
} */






#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>



void miniMap(FILE * inputFile, int (*func_ptr)(int,int))
{
   printf("%d\n", 100);

   func_ptr(2,3);

}
