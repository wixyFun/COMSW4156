int main()
{
 int main () {
   string s = "hello minimap";
   char c = 'm';
   char *ret;

   ret = strchr(s, c);

   printf("String after |%c| is - |%s|\n", c, ret);
   
   return(0);
}
