int main() {

	int[4] x;
	int[4] y;

	int i;
  int j;
  int l;
  
  l = len(x);
	for (i=0; i<l; i=i+1) {
		x[i] = 1;
		y[i] = 1;
	}
  x[0] = x[0]+1;

	print(x[0]);

}
