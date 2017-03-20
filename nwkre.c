#include <R.h>
#include <Rmath.h>

// integer m, integer n, double X of length n, double Y of length n, double g of length m, double res of length m

void nwkre (int *m, int *n, double *x, double *y, double *b, double *g, double *res){
	for(int i = 0; i < *m; i++){
		double sum1 = 0.0;
		double sum2 = 0.0;
		for(int j = 0; j < *n; j++){
			double temp = dnorm((x[j] - g[i])/ *b, 0, 1, 0)/ *b;
			sum1 += y[j] * temp;
			sum2 += temp;
		}
		if(sum2 > 0.0) res[i] = sum1 / sum2;
		else res[i] = 0.0;
	}
}