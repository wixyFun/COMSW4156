//
// Created by Ryan DeCosmo on 12/19/17.
//

#ifndef UNTITLED_NONTHREADED_H
#define UNTITLED_NONTHREADED_H

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

void miniMapNonThreaded(FILE* context, FILE* splits, int numberOfFiles, void (*mapper)(), void (*reducer)());
void map( FILE* file,FILE* context  );
void reduce(FILE* context, char** array);

#endif //UNTITLED_NONTHREADED_H
