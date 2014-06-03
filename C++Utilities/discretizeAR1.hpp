#ifndef _DISCRETIZE_AR1_HEADER 
#define _DISCRETIZE_AR1_HEADER  
class TDenseVector; 
class TDenseMatrix; 

int Tauchen(double rho, double mu_eps, double sigma_eps, int N, double m, TDenseVector &zvect, TDenseMatrix &Pmat);
int TauHuss(double rho, double mu_eps, double sigma_eps, int N, double Fixedsigma, TDenseVector &zvect, TDenseMatrix &Pmat); 
int Rouwenhurst(double rho, double mu_eps, double sigma_eps, int N, TDenseVector &zvect, TDenseMatrix &Pmat); 

#endif
