

int main()
{
  file myfile;
  myfile = open("bigbang.txt");
  minimap(myfile, split_by_quant(), map(), reduce() );

  return 0;
}

void map(file lilFilePtr)
{
  string key;
  key = "dog";
  string temp;
  int buf;
  buf = 10;

  while(fgets(temp,buf,lilFilePtr) != NULL)
  {
    string p;
    p = temp;
    while ((p = (strstr(p, key)))!= NULL)
    {
    count ++;
    ++p;
    }
  }
  return count;
}

int findKey(char *in, char *key, int buf){
int count = 0;
FILE *f;
f = fopen(in,"r");
char temp[buf];
while(fgets(temp,buf,f) != NULL){
    char *p = temp;
    while((p=(strstr(p,key)))!= NULL){
        count++;
        ++p;
    }
}
fclose(f);
return count;
}
