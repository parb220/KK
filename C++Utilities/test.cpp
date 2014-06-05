#include <iostream>
#include <cstdlib>
#include "kk_constant.hpp"
#include "addacooper1.hpp"
#include "chebyshev.hpp"
#include "mathutils.hpp"
#include "mherzo.hpp"
#include "dw_dense_matrix.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	/*double rho = 0.5, mu_eps = 0.0, sigma_eps = 1.0; 
	int N = 100; 
	TDenseVector zvect; 
	TDenseMatrix Pmat; 

	if (AddaCooper(rho, mu_eps, sigma_eps, N, zvect, Pmat) != SUCCESS)
		cout << "Error in AddaCooper" << endl; */
	// TDenseVector x = getPhi(0.3, 200); 
	
	// sorting
	/* TDenseVector x(10); 
	x.RandomUniform(); 
	TIndex sorted_order = sort(x); 
	cout << x; 
	for (int i=0; i<x.dim; i++)
		cout << x[sorted_order[i]] << " "; 
	cout << endl;*/

	TDenseVector x, w; 
	if (herzo(atoi(argv[1]), x, w) == SUCCESS)
		cout << x << endl << w << endl; 
}
