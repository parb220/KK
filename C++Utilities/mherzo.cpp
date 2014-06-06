#include <cmath>
#include "prcsn.h"
#include "dw_dense_matrix.hpp"
#include "kk_constant.hpp"
#include "mherzo.hpp"

using namespace std; 

int herzo(int N, TDenseVector &x, TDenseVector &w)
{
	if (x.dim != N)	
		x.Resize(N); 
	if (w.dim != N)
		w.Resize(N); 
	x.Zeros(); 
	w.Zeros(); 

	double hN = 1.0/N, zL=-1.1611+1.46*sqrt((double)N), z, z0; 
	for (int nR=0; nR < N/2; nR++)
	{
		if (nR == 0) 
			z = zL; 
		else 
			z = z - hN*(N/2.0-nR);

		int it = 0;
		double hD = 0.0;   
		do {
			it ++; 
			z0 = z; 
			double f0 = 1.0, f1 = 2.0 * z, hF=0.0; 
			for (int k=1; k<N; k++)
			{
				hF = 2.0 * z * f1 - 2.0 * k * f0; 
				hD = 2.0 * (k+1.0) * f1; 
				f0 = f1; 
				f1 = hF; 
			}
			double p = 1.0; 
			for (int i=0; i<nR; i++)
				p *= (z-x[i]); 
			double fD = hF/p, q = 0.0; 
			for (int i=0; i<nR; i++)
			{
				double wP = 1.0; 
				for (int j=0; j<nR; j++)
				{
					if (j != i)
						wP *= (z - x[j]); 
				}
				q += wP; 
			}
			double gD = (hD - q * fD) /p; 
			z = z - fD/gD; 
		} while (it < 40 && fabs(z-z0)/z>MACHINE_EPSILON);  
		x[nR] = z; 
		x[N-1-nR] = -z; 
		double r = 1.0; 
		for (int k=0; k<N; k++)
			r *= 2.0*(k+1.0); 
		w[nR] = 3.544907701811*r/(hD*hD);
		w[N-1-nR] = w[nR]; 	
	}	
	if (N != 2*(N/2)) // if N is odd
	{
		double r1=1.0, r2=1.0; 
		for (int j=0; j<N; j++)
		{
			r1 *= 2.0*(j+1.0); 
			if ( j >= (N-1)/2) 
				r2 *= (j+1.0);
		}
		w[(N-1)/2] = 0.88622692545276*r1/(r2*r2);
		x[(N-1)/2] = 0.0;
	}
	return SUCCESS; 
}
