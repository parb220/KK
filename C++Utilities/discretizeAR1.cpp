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

	TDenseVector zvect = linspace(mu_z-m*sigma_z,mu_z+m*sigma_z,N);
	double w = (zvect[1]-zvect[0])/2.0; 

	for (int i=0; i<N; i++)
	{
		Pmat(i,0) = enordf((zvect[0] + w - rho*zvect[i] - mu_eps)/sigma_eps); 
		for (int j=1; j<N-1; j++)
			Pmat(i,j) = enordf((zvect[j] + w- rho*zvect[i] - mu_eps)/sigma_eps) - enordf((zvect[j] - w- rho*zvect[i] - mu_eps)/sigma_eps); 
		Pmat(i,N-1) = 1.0 - enordf((zvect[N-1] - w - rho*zvect[i] - mu_eps)/sigma_eps); 
	}
	return SUCCESS; 
}

int TauHuss(double rho, double mu_eps, double sigma_eps, int N, double Fixedsigma, TDenseVector &zvect, TDenseMatrix &Pmat)
{
	if (zvect.dim != N)
		zvect.Resize(N); 
	if (Pmat.rows != N || Pmat.cols != N)
		Pmat.Resize(N,N); 
	
	double mu_z = mu_eps/(1.0-rho); 
	double sigma_z = sigma_eps/sqrt(1.0-rho*rho); 
	double sigma_zcond = sigma_eps; 

	
}
