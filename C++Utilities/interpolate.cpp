#include <cmath>
#include <algorithm>
#include "prcsn.h"
#include "dw_dense_matrix.hpp"
#include "kk_constant.hpp"
#include "interpolate.hpp"

using namespace std; 
// find the location of x1 in xvect so that xvect(jl) < x1 < xvect(jl+1)
// Need to check when xvect is in a descending order
int Locate(const TDenseVector &xvect, double x, int &loc)
{
	bool ascending = true; 
	if (xvect[0] <= xvect[xvect.dim-1]) // ascending order
	{
		for (int i=0; i<xvect.dim-1; i++)
		{
			if (xvect[i] > xvect[i+1])
				return ERROR; 
		}
	}
	else  // descending order
	{
		for (int i=0; i<xvect.dim-1; i++)
		{
			if (xvect[i] <= xvect[i+1])
				return ERROR; 
		}
		ascending = false; 
	}
	if (ascending)
	{
		int up = upper_bound(xvect.vector, xvect.vector+xvect.dim, x)-xvect.vector; // xvect[up-1] <= x < xvect[up]
		loc = up - 1; 
	}
	else 
	{
		// When descending, we need to reverse the order of xvect
		// xvect_reverse is the reverse version of xvect
		TDenseVector xvect_reverse(xvect.dim,0.0); 
		xvect_reverse.CopyContent(xvect); 
		reverse(xvect_reverse.vector, xvect_reverse.vector+xvect_reverse.dim); 
		int up = upper_bound(xvect_reverse.vector, xvect_reverse.vector+xvect_reverse.dim, x)-xvect_reverse.vector; 
		// xvect_reverse[up-1] <= x < xvect_reverse[up]
		// xvec[n-up] <= x < xvect[n-(up+1)]
		loc = xvect.dim-(up+1); 
	}
	return SUCCESS; 
}

int Locate(const TIndex &xvect, int x, int &loc)
{
	bool ascending = true; 
	if (xvect[0] <= xvect[xvect.size-1]) // ascending order
	{
		for (int i=0; i<xvect.size-1; i++)
		{
			if (xvect[i] > xvect[i+1])
				return ERROR; 
		}
	}
	else  // descending order
	{
		for (int i=0; i<xvect.size-1; i++)
		{
			if (xvect[i] <= xvect[i+1])
				return ERROR; 
		}
		ascending = false; 
	}
	if (ascending)
	{
		int up = upper_bound(xvect.index, xvect.index+xvect.size, x)-xvect.index; // xvect[up-1] <= x < xvect[up]
		loc = up - 1; 
	}
	else 
	{
		// When descending, we need to reverse the order of xvect
		// xvect_reverse is the reverse version of xvect
		TIndex xvect_reverse(xvect);  
		reverse(xvect_reverse.index, xvect_reverse.index+xvect_reverse.size); 
		int up = lower_bound(xvect_reverse.index, xvect_reverse.index+xvect_reverse.size, x)-xvect_reverse.index; 
		// xvect_reverse[up-1] < x <= xvect_reverse[up]
		// <=> xvect[n-(up+1)] >= x > xvec[n-up]	
		loc = xvect.size-(up+1); 
	}
	return SUCCESS; 
}

int get_interps(const TDenseVector &xvect, double x, int &loc, double &mx)
{	
	// check if xvect is ordered
	if (xvect[0] <= xvect[xvect.dim-1])
	{
		for (int i=0; i<xvect.dim-1; i++)
		{
			if (xvect[i] > xvect[i+1])
				return ERROR; 
		}
	}
	else 
	{
		for (int i=0; i<xvect.dim-1; i++)
		{
			if (xvect[i] <= xvect[i+1])
				return ERROR; 
		}
	}
	
	if(fabs(x-xvect[0]) < MACHINE_EPSILON)
		mx = 0.0; 
	else if (xvect[0] >= x)
		loc = 0; 
	else if (xvect[xvect.dim-1] <= x)
		loc = xvect.dim - 2; 
	else if (Locate(xvect, x, loc) != SUCCESS)
		return ERROR; 
	mx = (x-xvect[loc])/(xvect[loc+1]-xvect[loc]); 
	return SUCCESS; 
}

int Interpolate(const TDenseVector &xvect, const TDenseVector &yvect, const TDenseVector &x1vect, TDenseVector &y1vect)
{
	double slope, inter; 
	if (y1vect.dim != x1vect.dim)
		y1vect.Resize(x1vect.dim); 
	int loc; 
	for (int i=0; i<x1vect.dim; i++)
	{
		if (x1vect[i] < xvect[0]) 
		{
			slope = (yvect[1]-yvect[0])/(xvect[1]-xvect[0]); 
			inter = yvect[1] - slope * xvect[1]; 
		}
		else if (x1vect[i] > xvect[xvect.dim-1])
		{
			slope = (yvect[yvect.dim-1]-yvect[yvect.dim-2])/(xvect[xvect.dim-1]-xvect[xvect.dim-2]); 
			inter = yvect[yvect.dim-1] - slope * xvect[xvect.dim-1]; 
		}
		else 
		{
			if (Locate(xvect, x1vect[i], loc) != SUCCESS)
				return ERROR; 
			slope = (yvect[loc+1] - yvect[loc]) / (xvect[loc+1] - xvect[loc]); 
			inter = yvect[loc] - slope * xvect[loc]; 		
		}
		y1vect[i] = slope * x1vect[i] + inter; 
	}
	return SUCCESS;  
}

int Interpolate(const TDenseVector &xvect, const TDenseVector &yvect, double x1, double &y1)
{
	double slope, inter; 
	if (x1 == xvect[0]) 
		y1 = yvect[0]; 
	else if (x1 >= xvect[xvect.dim-1])
		y1 = yvect[xvect.dim-1]; 
	else if (x1 < xvect[0])
	{
		slope = (yvect[1]-yvect[0])/(xvect[1]-xvect[0]); 
		inter = yvect[1] - slope * xvect[1]; 
		y1 = inter + slope * x1; 
	}
	else if (xvect.dim == 2) 
	{
		slope = (yvect[1]-yvect[0])/(xvect[1]-xvect[0]);
                inter = yvect[0] - slope * xvect[0];
                y1 = inter + slope * x1;
	}
	else 
	{
		int location; 
		if (Locate(xvect, x1, location) != SUCCESS)
			return ERROR; 
		if (fabs(xvect[location]-x1) < MACHINE_EPSILON) 
			y1 = yvect[location]; 
		else 
		{
			slope = (yvect[location+1]-yvect[location])/(xvect[location+1]-xvect[location]); 
			inter = yvect[location] - slope * xvect[location]; 
			y1 = inter + slope * x1; 
		}
	}
	return SUCCESS; 
}
