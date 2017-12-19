//
// Created by Ryan DeCosmo on 12/19/17.
//

#include "NonThreaded.h"




void mapNonThreaded( FILE* file ){

    char* word = "Inside Mapper";
    printf("%s\n", word);
}

void reduceNonThreaded(FILE* file){
    char* word = "Inside Reducer";
    printf("%s\n", word);

}

void miniMapNonThreaded(FILE* context, FILE* splits, int numberOfFiles, void (*mapper)(), void (*reducer)()){
   int MAXFILE = 1024;

    int numFiles =  numberOfFiles;
    printf("numFiles: %i\n", numFiles );

    /*
     * MAPPER
     * This block will tokenize and map into outfile*/
    char delimit[]=" \t\r\n\v\f-,:;."; //const char s = ' ';
    char *token;

    if (splits != NULL) {
        char line [1000];
        while(fgets(line,sizeof line,splits)!= NULL)  {
            fprintf(stdout,"%s",line);

            char* one = "1";
            token = strtok(line, delimit);
            int i = 0;
            while( token  ) {
                fprintf(context,"%s,%s\n", token, one);
                token = strtok(NULL, delimit);
            }
        }
    }
    else {
        perror(splits);
    }
//////////////////



///////////////////
    fclose(splits);

    //REDUCER

}

static int myCompare (const void * a, const void * b)
{
    return strcmp (*(const char **) a, *(const char **) b);
}

void sort(const char *arr[], int n)
{
    qsort (arr, n, sizeof (const char *), myCompare);
}

