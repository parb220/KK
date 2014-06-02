#ifndef ADD_A_COOPER_HEADER
#define ADD_A_COOPER_HEADER
#include <gsl/gsl_roots.h>

class TDenseVector; 
class TDenseMatrix; 


double QAG_Integration(gsl_function *f, double a, double b, double epsabs, double epsrel, double &error);  
double QAGIL_Integration(gsl_function *f, double b, double epsabs, double epsrel, double &error); 
double QAGIU_Integration(gsl_function *f, double a, double epsabs, double epsrel, double &error); 

class FAddaCooper
{
public:
	static int N; 
	static double sigma_z; 
	static double mu_z;
	static double sigma_eps; 
	static double rho; 
	static double e1; 
	static double e2; 
	
	static double function(double x, void *p); 
}; 

int AddaCooper(double rho, double mu_eps, double sigma_eps, int N, TDenseVector &zvect, TDenseMatrix &Pmat);  

#endif
