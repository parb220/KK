#include <cmath>
#include "kk_constant.hpp"
#include "dw_dense_matrix.hpp"
#include "chebyshev.hpp"

using namespace std; 

// Evaluates the 1st S Cheybshev polynomials at x
TDenseVector getPhi(double x, int S)
{
	if (S <= 0)
 		return TDenseVector(0); 

	TDenseVector phi(S,1.0); 
	if (S == 1)
		return phi; 
	phi[1] = x; 
	for (int i=2; i<S; i++)
	{
		phi[i] = 2*x*phi[i-1]-phi[i-2]; 
		phi[i] = phi[i] < 1.0 ? phi[i] : 1.0; 
		phi[i] = phi[i] > -1.0 ? phi[i] : -1.0; 
	}
	return phi; 
}

// Evaluates the 1st S Cheybshev polynomials at each point in xvect
TDenseMatrix getPhi(const TDenseVector &x, int S)
{
	if (x.dim <= 0 || S <= 0)
		return TDenseMatrix(0,0); 

	TDenseMatrix phi(x.dim, S, 1.0); 
	if (S == 1)
		return phi; 
	phi.InsertColumnMatrix(0,1,x); // phi(:,1) = x; 
	for (int i=2; i<S; i++)
	{
		phi.InsertColumnMatrix(0,i,(2.0*DotMultiply(x,phi.ColumnVector(i-1))-phi.ColumnVector(i-2)));
		for (int j=0; j<x.dim; j++)
		{
			phi(j,i) = phi(j,i) < 1.0 ? phi(j,i) : 1.0; 
			phi(j,i) = phi(j,i) > -1.0 ? phi(j,i) : 1.0; 
		}
	}
	return phi; 
}

// Evaluates the nth Cheybshev polynomials at each point in xvect
TDenseVector getCheb(const TDenseVector &x, int n)
{
	TDenseVector cheb(x.dim,0.0); 
	for (int i=0; i< x.dim; i++)
		cheb[i] = cos((n-1.0)*acos(x[i])); 
	return cheb; 
}

// Returns the s extrema of the of sth Chebyshev polynomial (also called the Gauss-Lobatto nodes)
TDenseVector getExtrema(int s)
{
	TDenseVector extrema(s,0.0); 
	for (int j=0; j<s; j++)
		extrema[j] = -cos(PI*j/(s-1.0)); 
	return extrema; 
}

// Returns the s roots of the of sth Chebyshev polynomial (also called the Gauss-Lobotto nodes)
TDenseVector getRoots(int s)
{
	TDenseVector roots(s,0.0); 
	for (int i=0; i<s; i++)
		roots[i] = -cos( (2.0*i+1.0)*PI/(2.0*s)); 
	return roots; 
}

TDenseVector getPhiScalar(double x, int S) { return getPhi(x,S); }
TDenseMatrix getPhiVect(const TDenseVector &x, int S) { return getPhi(x,S); }
