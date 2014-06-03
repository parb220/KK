#ifndef _CHEBYSHEV_HEADER_
#define _CHEBYSHEV_HEADER_
class TDenseVector; 

// Evaluates the 1st S Cheybshev polynomials at x
TDenseVector getPhi(double x, int S); 
TDenseVector getPhiScalar(double x, int S); 

// Evaluates the 1st S Cheybshev polynomials at each point in xvect
TDenseMatrix getPhi(const TDenseVector &x, int S);  
TDenseMatrix getPhiVect(const TDenseVector &x, int S);  

// Evaluates the nth Cheybshev polynomials at each point in xvect
TDenseVector getCheb(const TDenseVector &x, int n); 

// Returns the s extrema of the of sth Chebyshev polynomial (also called the Gauss-Lobatto nodes)
TDenseVector getExtrema(int s); 

// Returns the s roots of the of sth Chebyshev polynomial (also called the Gauss-Lobotto nodes)
TDenseVector getRoots(int s); 
#endif
