#ifndef _MHERZO_HEADER_
#define _MHERZO_HEADER_

class TDenseVector; 

// Compute the zeros of Hermite polynomial Ln(x) in the interval [-ì,ì], and the corresponding
// weighting coefficients for Gauss-Hermite integration

int herzo(int n, TDenseVector &x, TDenseVector &w); 
#endif
