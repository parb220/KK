#include <cmath>
#include "dw_dense_matrix.hpp"
#include "kk_constant.hpp"
#include "mathutils.hpp"
#include "mherzo.hpp"
#include "distributions.hpp"
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

	zvect = linspace(mu_z-m*sigma_z,mu_z+m*sigma_z,N);
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

	TDenseVector xvect(N,0.0), wvect(N,0.0); 
	int returned_code = herzo(N,xvect, wvect);
	if (returned_code != SUCCESS)
		return returned_code; 
	xvect = -1.0/100.0*xvect; 
	wvect = 1.0/100 * wvect; 
	zvect = sqrt(2.0) * 100 * Fixedsigma * xvect + mu_z * Ones(zvect.dim); 

	TDenseMatrix zMat(N,N,0.0), wMat(N,N,0.0); 
	for (int i=0; i<zMat.cols; i++)
		zMat.InsertColumnMatrix(0,i,zvect); // zMat = spread(zvect, 2, N) along column
	for (int i=0; i<wMat.rows; i++)
		wMat.InsertRowMatrix(i,0,wvect); // wMat = spread(wvect,1, N) along row
	TDenseMatrix z1Mat = Transpose(zMat); 

	TDenseMatrix f_zjcondzi = 1.0/(sqrt(2.0)*sigma_zcond)*(z1Mat-rho*zMat-mu_eps*Ones(N,N)); 
	for (int i=0; i<f_zjcondzi.rows; i++)
		for (int j=0; j<f_zjcondzi.cols; j++)
			f_zjcondzi(i,j) = exp(-f_zjcondzi(i,j)*f_zjcondzi(i,j))/(sqrt(2.0*PI)*sigma_zcond); 
        TDenseMatrix f_zj = 1.0/(sqrt(2.0)*Fixedsigma)*(z1Mat-(rho*mu_z-mu_eps)*Ones(N,N)); 
	for (int i=0; i<f_zj.rows; i++)
		for (int j=0; j<f_zj.cols; j++)
			f_zj(i,j) = exp(-f_zj(i,j)*f_zj(i,j))/(sqrt(2.0*PI)*Fixedsigma); 
	
	TDenseVector colSum(Pmat.rows,0.0); // sum along columns
	for (int i=0; i<Pmat.rows; i++)
	{
		for (int j=0; j<Pmat.cols; j++)
		{
			Pmat(i,j) = 1.0/sqrt(PI) * wMat(i,j) * f_zjcondzi(i,j) / f_zj(i,j);
			colSum[i] = colSum[i] + Pmat(i,j); 
		}
	}
	TDenseMatrix colSumSpread(Pmat.rows, N, 0.0); 
	for (int i=0; i<colSumSpread.cols; i++)
		colSumSpread.InsertColumnMatrix(0, i, colSum); // colSumSpread = spread(colSum, 2, N); 
	for (int i=0; i<Pmat.rows; i++)
		for (int j=0; j<Pmat.cols; j++)
			Pmat(i,j) = Pmat(i,j) / colSumSpread(i,j); 

	return SUCCESS; 
}

int Rouwenhurst(double rho, double mu_eps, double sigma_eps, int N, TDenseVector &zvect, TDenseMatrix &Pmat)
{
	if (zvect.dim != N)
		zvect.Resize(N); 
	if (Pmat.rows != N || Pmat.cols != N)
		Pmat.Resize(N,N);
 
	double mu_z = mu_eps/(1.0-rho); 
	double sigma_z = sigma_eps/sqrt(1.0-rho*rho); 
	double q = (rho+1.0)/2.0; 
	double eps = sqrt(N-1.0) * sigma_z; 

	if (N == 1)
	{
		Pmat.Ones(); 
		zvect = mu_z * Ones(zvect.dim); 
	}
	else if (N == 2)
	{
		Pmat(0,0) = Pmat(1,1) = q; 
		Pmat(0,1) = Pmat(1,0) = 1.0-q; 
		zvect[0] = mu_z - eps; 
		zvect[1] = mu_z + eps; 
	}
	else 
	{
		TDenseMatrix P1(2,2), P2; 
		P1(0,0) = P1(1,1) = q; 
		P1(0,1) = P1(1,0) = 1.0-q; 
		for (int i=2; i<N; i++)
		{
			P2.Resize(i+1,i+1);
			P2.Zeros(); 
			P2.Insert(0,0,q*P1); 
			P2.SubMatrix(0,P1.rows-1,1,P1.rows) = P2.SubMatrix(0,P1.rows-1,1,P1.rows) + (1.0-q)*P1; 
			P2.SubMatrix(1,P1.rows,0,P1.cols-1) = P2.SubMatrix(1,P1.rows,0,P1.cols-1) + (1.0-q)*P1; 
			P2.SubMatrix(1,P1.rows,1,P1.cols) = P2.SubMatrix(1,P1.rows,1,P1.cols) + q*P1; 
			P2.SubMatrix(1,i-1,0,P2.cols-1) = 0.5*P2.SubMatrix(1,i-1,0,P2.cols-1); 
			if (i == N-1)
				Pmat = P2; 
			else 
			{
				P1.Resize(i+1,i+1); 
				P1.CopyContent(P2); 
			}	
		}
	}
	zvect = linspace(mu_z-eps, mu_z+eps, N); 
	return SUCCESS; 
}
