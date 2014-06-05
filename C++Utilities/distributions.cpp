#include <cmath>
#include "kk_constant.hpp"
#include "dw_dense_matrix.hpp"
#include "distributions.hpp"
#include "toms462.hpp"

using namespace std; 

double erfcc(double x)
{
	TDenseVector coef(10,0.0); 
	coef[0] = -1.26551223; 
	coef[1] = 1.00002368; 
	coef[2] = 0.37409196; 
	coef[3] = 0.09678418; 
	coef[4] = -0.18628806; 
	coef[5] = 0.27886807; 
	coef[6] = -1.13520398; 
	coef[7] = 1.48851587; 
	coef[8] = -0.82215223; 
	coef[9] = 0.17087277; 

	double z = fabs(x); 
	double t = 1.0/(1.0+0.5*z); 
	double value = t * exp(-z*z+poly(t, coef)); 
	if (x < 0)
		value = 2.0 - value; 
	return value; 
}

TDenseVector erfcc(const TDenseVector &x)
{
	TDenseVector coef(10,0.0);
        coef[0] = -1.26551223;
        coef[1] = 1.00002368; 
        coef[2] = 0.37409196; 
        coef[3] = 0.09678418; 
        coef[4] = -0.18628806; 
        coef[5] = 0.27886807; 
        coef[6] = -1.13520398; 
        coef[7] = 1.48851587; 
        coef[8] = -0.82215223; 
        coef[9] = 0.17087277;

	TDenseVector t(x.dim, 0.0), z(x.dim, 0.0); 
	for (int i=0; i<x.dim; i++)
	{
		z[i] = fabs(x[i]); 
		t[i] = 1.0/(1.0+0.5*z[i]); 
	}
	// intermediate_value = -z*z+poly(t,coef)
	TDenseVector intermediate_value = -1.0*DotMultiply(z,z)+poly(t,coef); 
	// intermediate_value = exp(-z*z+poly(t,coef))
	for (int i=0; i<intermediate_value.dim; i++)
		intermediate_value[i] = exp(intermediate_value[i]); 
	// value = t * exp(-z*z+poly(t,coef))
	TDenseVector value = DotMultiply(t, intermediate_value); 
	TIndex negative_x_index; 
	for (int i=0; i<value.dim; i++)
	{
		if (x[i] < 0)
			negative_x_index += i; 
	}
	value(negative_x_index) = 2.0*Ones(negative_x_index.Size()) - value(negative_x_index);  
	return value; 
}

TDenseVector erfcc_v(const TDenseVector &x) { return erfcc(x); }

double poly(double x, const TDenseVector &coeffs)
{
	double value; 
	if (coeffs.dim <= 0)
		value = 0.0; 
	else if (coeffs.dim < 8) 
	{
		value = coeffs[coeffs.dim-1]; 
		for (int i=coeffs.dim-2; i>=0; i--)
			value = x*value+coeffs[i]; 
	}
	else 
	{
		TDenseVector vec(coeffs.dim+1, 0.0); 
		double pow = x; 
		vec.Insert(0,coeffs); // vec(0:n-1) = coeffs; 
		int n = coeffs.dim; 
		int nn  = 0; 
		do {
			vec[vec.dim-1] = 0.0; 
			nn = (n+1) >> 1;	// right shift n+1 by 1 bit 
			vec.Insert(0, vec.SubVector(TIndex(0,2,n-1))+pow*vec.SubVector(TIndex(1,2,n)) ); 
			pow = pow *pow; 
			n = nn; 
		} while (nn != 1); 
		value = vec[0]; 
	}
	return value; 
}

double poly_rr(double x, const TDenseVector &coeffs) { return poly(x, coeffs); }

TDenseVector poly(const TDenseVector &x, const TDenseVector &coeffs)
{
	TDenseVector value(x.dim, 0.0); 
	if (coeffs.dim <= 0)
		value.Zeros(); 
	else if (coeffs.dim <= x.dim || coeffs.dim < 8)
	{
		value = coeffs[coeffs.dim-1] * Ones(value.dim); 	
		for (int i=coeffs.dim-2; i>=0; i--)
			value = DotMultiply(x, value) + coeffs[i] * Ones(value.dim); 
	}
	else
	{
		for (int i=0; i<value.dim; i++)
			value[i] = poly(x[i], coeffs); 
	}		
	return value; 
}

TDenseVector poly_rrv(const TDenseVector &x, const TDenseVector &coeffs) { return poly(x,coeffs); }

double enordf(double x) { return erfcc(-x/sqrt(2.0))/2.0; }

TDenseVector enordf(const TDenseVector &x) { return 0.5*erfcc(-1.0/sqrt(2.0)*x); }

TDenseVector enordf_v(const TDenseVector &x) { return enordf(x); }

TDenseMatrix GetInitialDistribution(const TDenseVector &grid1, const TDenseVector &grid2, double stdev1, double stdev2, double corr)
{
	TDenseMatrix dist(grid1.dim, grid2.dim, 0.0); 
	double w1 = (grid1[1]-grid1[0])/2.0, w2 = (grid2[1]-grid2[0])/2.0; 
	for (int i=1; i<grid1.dim-1; i++)
		for (int j=1; j<grid2.dim-1; j++)
			dist(i,j) = bivnor((grid1[i]-w1)/stdev1, (grid2[j]-w2)/stdev2, corr) - bivnor((grid1[i]+w1)/stdev1, (grid2[j]-w2)/stdev2, corr) - bivnor((grid1[i]-w1)/stdev1, (grid2[j]+w2)/stdev2, corr) + bivnor((grid1[j]+w1)/stdev1, (grid2[j]+w2)/stdev2, corr); 

	for (int i=0; i<grid1.dim; i++)
	{
		dist(i,0) = (1.0-enordf( (grid1[i] - w1)/stdev1 )) - (1.0-enordf( (grid1[i] + w1)/stdev1 )) - bivnor((grid1[i]-w1)/stdev1,(grid2[0]+w2)/stdev2,corr) + bivnor((grid1[i]+w1)/stdev1,(grid2[0]+w2)/stdev2,corr); 
		dist(i,grid2.dim-1) = bivnor((grid1[i]-w1)/stdev1,(grid2[grid2.dim-1]-w2)/stdev2,corr) - bivnor((grid1[i]+w1)/stdev1,(grid2[grid2.dim-1]-w2)/stdev2,corr);
	}
		
	for (int j=0; j<grid2.dim; j++)
	{
		dist(0,j) = (1.0-enordf( (grid2[j] - w2)/stdev2 )) - (1.0-enordf( (grid2[j] + w2)/stdev2 )) - bivnor((grid1[0]+w1)/stdev1,(grid2[j]-w2)/stdev2,corr) + bivnor((grid1[0]+w1)/stdev1,(grid2[j]+w2)/stdev2,corr); 
		dist(grid1.dim-1,j) = bivnor((grid1[grid1.dim-1]-w1)/stdev1,(grid2[j]-w2)/stdev2,corr) - bivnor((grid1[grid1.dim-1]-w1)/stdev1,(grid2[j]+w2)/stdev2,corr); 
	}

	dist(0,0) = 1.0 - (1.0-enordf( (grid1[0] + w1)/stdev1 )) - (1.0-enordf( (grid2[0] + w2)/stdev2 )) + bivnor((grid1[0]+w1)/stdev1,(grid2[0]+w2)/stdev2,corr);
	dist(0,grid2.dim-1) = (1.0-enordf( (grid2[grid2.dim-1] - w2)/stdev2 )) - bivnor((grid1[0]+w1)/stdev1,(grid2[grid2.dim-1]-w2)/stdev2,corr); 
        dist(grid1.dim-1,1) = (1.0-enordf( (grid1[grid1.dim-1] - w1)/stdev1 )) - bivnor((grid1[grid1.dim-1]-w1)/stdev1,(grid2[0]+w2)/stdev2,corr);
        dist(grid1.dim-1,grid2.dim-1) = bivnor((grid1[grid1.dim-1]-w1)/stdev1,(grid2[grid2.dim-1]-w2)/stdev2,corr);
	return dist; 
}
