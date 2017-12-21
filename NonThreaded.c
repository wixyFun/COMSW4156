
/*
@Ryan DeCosmo
@Olessya Medvedeva
*/

#include "NonThreaded.h"


//compare for string functions
static int compare (const void * a, const void * b)
{
    return strcmp (*(const char **) a, *(const char **) b);
}


void tokenizeFile(FILE* splits, FILE* context){
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
        perror("Error in mapper file");
    }
    rewind(context);

}

void countUniqueTokens(FILE* context, char** array){
    rewind(context);
    int totalstrings = 300;
    int stringsize = 50;
    char** words ;
    int freq[300];

    for (int i = 0; i < totalstrings; i++) {
        array[i] = (char *)malloc(stringsize);
    }

    char delimit[]=",1"; //const char s = ' ';
    char *token;

    if (context != NULL) {
        char line [1000];
        int j = 0;
        while(fgets(line,sizeof line,context)!= NULL)  {
            token = strtok(line, delimit);
            strcpy(array[j],token);
            token = strtok(NULL, delimit);
            j++;
        }
    }
    else {
        perror("Error");
    }
    //sort the array
    qsort (array, totalstrings, sizeof (const char *), compare);

    words = malloc(sizeof(char*)*totalstrings);

    for (int i = 0; i < totalstrings; i++) {
        words[i] = (char *)malloc(stringsize);
        freq[i] = 0;
    }

    //accumulator
    char** ptr = array;
    int counts =0;
    int i = 0;
    words[0] = *ptr;

    while(*ptr != 0) {
        // printf("%s \n", *ptr);
        if (strcmp(words[i], *ptr) == 0) {
            words[i] = *ptr;
            ++ptr;
            freq[i] = freq[i] + 1;
        } else {
            i++;
            words[i] = *ptr;
            freq[i] = freq[i] + 1;
            ++ptr;
        }
    }

    for (int i = 0; i < sizeof(freq) / sizeof(freq[0]); i++) {
        printf("Word: %s , Freq  :%i \n", words[i], freq[i]);
    }
}





void map(FILE* splits, FILE* context){
    //built in function to tokenize mini files
    tokenizeFile(splits,context);
}



void reduce(FILE* context, char** array){
    //function to accumulate values of the context file
    countUniqueTokens(context,array);
}




void miniMapNonThreaded(FILE* context, FILE* splits, int numberOfFiles, void (*mapper)(), void (*reducer)()){


    char **array = malloc(300 * sizeof(char *));

    map(splits,context);
    reduce(context,array);

    fclose(splits);
    fclose(context);

}
