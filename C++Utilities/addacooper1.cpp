#include <cmath>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_integration.h>
#include "dw_dense_matrix.hpp"
#include "kk_constant.hpp"
#include "addacooper1.hpp"

using namespace std;

double QAG_Integration(gsl_function *f, double a, double b, double epsabs, double epsrel, double &error)
{
	gsl_integration_workspace * w = gsl_integration_workspace_alloc(INTEGRATION_LIMIT);
	double result; 
	int key = GSL_INTEG_GAUSS61; 
	gsl_integration_qag(f, a, b, epsabs, epsrel, INTEGRATION_LIMIT, key, w, &result, &error); 
	gsl_integration_workspace_free(w); 
	return result; 
}

double QAGIL_Integration(gsl_function *f, double b, double epsabs, double epsrel, double &error)
{
	gsl_integration_workspace *w = gsl_integration_workspace_alloc(INTEGRATION_LIMIT); 
	double result; 
	gsl_integration_qagil(f, b, epsabs, epsrel, INTEGRATION_LIMIT, w, &result, &error); 
	gsl_integration_workspace_free(w);
	return result;  
}

double QAGIU_Integration(gsl_function *f, double a, double epsabs, double epsrel, double &error)
{
	gsl_integration_workspace *w = gsl_integration_workspace_alloc(INTEGRATION_LIMIT); 
	double result; 
	gsl_integration_qagiu(f, a, epsabs, epsrel, INTEGRATION_LIMIT, w, &result, &error); 
	gsl_integration_workspace_free(w); 
	return result; 
}

int FAddaCooper::N;
double FAddaCooper::sigma_z;
double FAddaCooper::mu_z;
double FAddaCooper::sigma_eps;
double FAddaCooper::rho;
double FAddaCooper::e1;
double FAddaCooper::e2;

double FAddaCooper::function(double x, void *p)
{
	double value = N/sqrt(2.0*PI*sigma_z*sigma_z) * (exp(-(x-mu_z)*(x-mu_z)/(2.0*sigma_z*sigma_z)) * (gsl_cdf_ugaussian_P((e2-mu_z*(1-rho)-rho*x)/sigma_eps) - gsl_cdf_ugaussian_P((e1-mu_z*(1-rho)-rho*x)/sigma_eps) ) ); 
	return value; 
}

int AddaCooper(double rho, double mu_eps, double sigma_eps, int N, TDenseVector &zvect, TDenseMatrix &Pmat)
{
	if (zvect.dim != N)
		zvect.Resize(N); 
	if (Pmat.rows != N || Pmat.cols != N)
		Pmat.Resize(N,N); 	

	double errabs = 1.0e-12, errest;

	double mu_z = mu_eps/(1.0-rho), sigma_z = sigma_eps/sqrt(1.0-rho*rho); 
	TDenseVector evect(N+1,0.0); 	
 	evect[0] = -1.0e6; 
	evect[N] = 1.0e6; 
	for (int i=1; i<N; i++)
		evect[i] = sigma_z * gsl_cdf_ugaussian_Pinv((double)i/(double)N) + mu_z; 
	for (int i=0; i<N; i++)
		zvect[i] = N*sigma_z*(gsl_ran_gaussian_pdf(evect[i]-mu_z, sigma_z) - gsl_ran_gaussian_pdf(evect[i+1]-mu_z, sigma_z)) + mu_z; 


	gsl_function f; 
	f.function = &(FAddaCooper::function); 
	f.params = NULL; 
	FAddaCooper::N = N; 
	FAddaCooper::sigma_z = sigma_z; 
	FAddaCooper::mu_z = mu_z; 
	FAddaCooper::sigma_eps = sigma_eps; 
	FAddaCooper::rho = rho; 

	for (int i=1; i<N-1; i++)
	{
		for (int j=0; j<N; j++)
		{
			FAddaCooper::e1 = evect[j]; 
			FAddaCooper::e2 = evect[j+1]; 
			
			Pmat(i,j) = QAG_Integration(&f, evect[i], evect[i+1], errabs, errabs, errest); 	
		}
	}

	for (int j=0; j<N; j++)
	{
		FAddaCooper::e1 = evect[j]; 
		FAddaCooper::e2 = evect[j+1]; 
		Pmat(0,j) = QAGIL_Integration(&f, evect[1], errabs, errabs, errest);
		Pmat(N-1,j) = QAGIU_Integration(&f, evect[N-1], errabs, errabs, errest);  
	}

	TDenseVector rowSum(Pmat.rows,0.0); 
	for (int i=0; i<N; i++)
	{
		rowSum[i] = InnerProduct(Pmat.RowVector(i),Ones(Pmat.cols));
		Pmat.InsertRowMatrix(i, 0, 1.0/rowSum[i]*Pmat.RowVector(i)); 
	}
	return SUCCESS; 
}
