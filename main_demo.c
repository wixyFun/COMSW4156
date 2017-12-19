#include <stdio.h>
#include "NonThreaded.h"
#include <stdlib.h>


int main() {

    int numFiles = 4;
    FILE** files = malloc(numFiles * sizeof(FILE *));
    FILE *fp =  fopen("/Users/ryandecosmo/CLionProjects/untitled/sample.txt", "r+");
    FILE *fpOut =  fopen("/Users/ryandecosmo/CLionProjects/untitled/out.txt", "w+");

    //split_by_quant(fp,5);
    //FILE** myFiles =  split_by_quant(fp,5); //split_by_size(fp,256);
    //FILE* myFiles =  split_by_size(fp,256);
    //printf("file: %s ",files2[0]);
    //split_by_size(fp,10);
    //miniMap(fp,myFiles, 5, map,reduce);
    miniMapNonThreaded(fpOut,fp, 1, mapNonThreaded,reduceNonThreaded );

    return 0;
}
