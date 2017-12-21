//
// Created by Ryan DeCosmo on 12/19/17.
//

#include "NonThreaded.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define BUF 128 /* can change the buffer size as well */
#define TOT 10 /* change to accomodate other sizes, change ONCE here */


static int compare (const void * a, const void * b)
{
    /* The pointers point to offsets into "array", so we need to
       dereference them to get at the strings. */

    return strcmp (*(const char **) a, *(const char **) b);
}



void mapNonThreaded( FILE* file ){

    char* word = "Inside Mapper";
    printf("%s\n", word);
}

void reduceNonThreaded(FILE* file){
    char* word = "Inside Reducer";
    printf("%s\n", word);

}

void map(FILE* splits, FILE* context){
    char delimit[]=" \t\r\n\v\f-,:;."; //const char s = ' ';
    char *token;

    if (splits != NULL) {
        char line [1000];
        while(fgets(line,sizeof line,splits)!= NULL)  {
            //  fprintf(stdout,"%s",line);
          //  char* one = "1";
            token = strtok(line, delimit);
            while( token  ) {
                fprintf(context,"%s \n", token ); //, one); //
                token = strtok(NULL, delimit);
            }
        }
    }
    else {
        perror(splits);
    }
    rewind(context);
}



void reduce(FILE* context, char** array){


    rewind(context);

    int totalstrings = 300;
    int stringsize = 50;


    //int i;
    for (int i = 0; i < totalstrings; i++) {
        array[i] = (char *)malloc(stringsize);
       // printf("array %i \n", sizeof(array[i]));
    }

    char delimit[]=",1"; //const char s = ' ';
    char *token;

    if (context != NULL) {
        char line [1000];
        int j = 0;
        while(fgets(line,sizeof line,context)!= NULL)  {
            //  fprintf(stdout,"%s",line);

            //char* one = "1";
            token = strtok(line, delimit);

           // while( token  ) {
            //for(int j = 0; j<100; j++){
               //printf("%s",token);
                //array[j] = (char *)malloc(sizeof(token));
                //array[j] = token;
                //strcpy(*array[j],token);
                printf("%s \n",token);
                strcpy(array[j],token);
                /*for(int s = 0; s < sizeof(token) ; s++){
                    array[j][s] = token+s;
                }*/

                //fprintf(context,"%s,%s\n", token, one);
               token = strtok(NULL, delimit);
            j++;
        }
    }
    else {
        perror(context);
    }

        qsort (array, 300, sizeof (const char *), compare);

        for (int i = 0; i < 300; i++) {
            printf ("%d: %s.\n", i, array[i]);
        }

    char** words;
    int freq[300];

    for (int i = 0; i < totalstrings; i++) {
        words = (char *)malloc(stringsize);
    }

    for (int i = 0; i < totalstrings; i++) {
        //freq[i] = (int)malloc(sizeof(int));
        freq[i] = 0;
    }

    int count = 1;
    char* tempWord = array[0];
    int j = 0;
      for (int i = 0; i < 300; i++) {
          if (strcmp(tempWord, array[i]) == 0) {
              count++;
              freq[j] = count;
          } else {
              words[j] = tempWord;
              freq[j]= count;
              tempWord = array[i];
              j = j+1;
              count = 0;
          }


  }

    for (int i = 0; i < 300; i++) {
       printf("Freq I :%i \n", freq[i]);
    }
    for (int i = 0; i < 300; i++) {
        printf("words I :%s \n", words[i]);
    }



    }








void miniMapNonThreaded(FILE* context, FILE* splits, int numberOfFiles, void (*mapper)(), void (*reducer)()){


    int numFiles =  numberOfFiles;
    printf("numFiles: %i\n", numFiles );

    char **array = malloc(300 * sizeof(char *));

    map(splits,context);
    reduce(context,array);


    fclose(splits);
    fclose(context);

}



