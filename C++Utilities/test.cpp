#include <iostream>
#include "kk_constant.hpp"
#include "addacooper1.hpp"
#include "dw_dense_matrix.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	double rho = 0.5, mu_eps = 0.0, sigma_eps = 1.0; 
	int N = 100; 
	TDenseVector zvect; 
	TDenseMatrix Pmat; 

	if (AddaCooper(rho, mu_eps, sigma_eps, N, zvect, Pmat) != SUCCESS)
		cout << "Error in AddaCooper" << endl; 
}
