/*
@Ryan DeCosmo
@Olessya Medvedeva
*/

#include <stdio.h>
#include "NonThreaded.h"
#include <stdlib.h>


int main() {

    int numFiles = 4;
    FILE** files = malloc(numFiles * sizeof(FILE *));
    FILE *fp =  fopen("sample.txt", "r+");
    FILE *fpOut =  fopen("out.txt", "w+");

    miniMapNonThreaded(fpOut,fp, 1, map,reduce );

    return 0;
}
