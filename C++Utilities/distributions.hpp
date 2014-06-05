#ifndef _DISTRIBUTIONS_HEADER_
#define _DISTRIBUTIONS_HEADER_

class TDenseVector; 
class TDenseMatrix; 

double erfcc(double x); 
double erfcc_s(double x); 

TDenseVector erfcc(const TDenseVector &x); 
TDenseVector erfcc_v(const TDenseVector &x); 

double poly(double x, const TDenseVector &coeffs); 
double poly_rr(double x, const TDenseVector &coeffs); 

TDenseVector poly(const TDenseVector &x, const TDenseVector &coeffs); 
TDenseVector poly_rrv(const TDenseVector &x, const TDenseVector &coeffs); 

double enordf(double x); 
double enordf_s(double x); 

TDenseVector enordf(const TDenseVector &x); 
TDenseVector enordf_v(const TDenseVector &x); 

TDenseMatrix GetInitialDistribution(const TDenseVector &grid1, const TDenseVector &grid2, double stdev1, double stdev2, double corr); 
#endif
