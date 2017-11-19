#include<stdio.h>

  void splitfile(char fileName[])
  {
    FILE *ptr_file;
		int x;

		ptr_file =fopen(fileName, "w");

		if (!ptr_file)
      printf("Error in splitfile\n");

		for (x=1; x<=10; x++)
    {
      // char j = x +'0';
      // putchar(j);
      printf("%d\n",x);
			fprintf(ptr_file,"%d\n", x);
    }

		fclose(ptr_file);

  }
