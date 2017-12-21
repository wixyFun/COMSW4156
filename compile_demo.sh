#!/bin/bash   
gcc -c NonThreaded.c 
gcc -c main_demo.c 
gcc NonThreaded.o main_demo.o -o main_demo
