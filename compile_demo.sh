#!/bin/bash
#Ryan DeCosmo
gcc -c NonThreaded.c
gcc -c main_demo.c
gcc NonThreaded.o main_demo.o -o main_demo
