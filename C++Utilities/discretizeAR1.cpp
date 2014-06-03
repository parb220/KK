#include <cmath>
#include "dw_dense_matrix.hpp"
#include "kk_constant.hpp"
#include "discretizeAR1.hpp"

using namespace std; 

int Tauchen(double rho, double mu_eps, double sigma_eps, int N, double m, TDenseVector &zvect, TDenseMatrix &Pmat)
{
	if (zvect.dim != N)
		zvect.Resize(N); 
	if (Pmat.rows != N || Pmat.cols != N)
		Pmat.Resize(N,N); 
	
	double mu_z = mu_eps/(1.0-rho); 
	double sigma_z = sigma_eps/sqrt(1.0-rho*rho); 
	double sigma_zcond = sigma_eps; 

}
