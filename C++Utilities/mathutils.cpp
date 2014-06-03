#include <cmath>
#include <algorithm>
#include <vector>
#include "dw_dense_matrix.hpp"
#include "kk_constant.hpp"
#include "mathutils.hpp"

using namespace std; 

TDenseMatrix kron(const TDenseMatrix &A, const TDenseMatrix &B) { return Kron(A,B); }
TDenseMatrix kronMat(const TDenseMatrix &A, const TDenseMatrix &B) { return kron(A,B); }
TDenseVector kron(const TDenseVector &A, const TDenseVector &B) { return Kron(A,B); }
TDenseVector kronVect(const TDenseVector &A, const TDenseVector &B) { return kron(A,B); }

TDenseVector linspace(double xmin, double xmax, int n)
{
	TDenseVector x(n, 0.0); 
	for (int i=0; i<n; i++)
		x[i] = xmin + (xmax-xmin)*i/(n-1.0); 
	return x; 
}

bool Comparator(const NumberWithOrder &lhs, const NumberWithOrder &rhs)
{
        return lhs.Number() < rhs.Number();
}

TIndex sort(const TDenseVector &x)
{
	vector<NumberWithOrder> x_order(x.dim); 
	for (int i=0; i<x.dim; i++)
		x_order[i] = NumberWithOrder(x[i], i); 

	sort(x_order.begin(), x_order.end(), Comparator); 
	TIndex sorted_index; 
	for (int i=0; i<x.dim; i++)
		sorted_index += x_order[i].Order(); 
	return sorted_index; 
}
