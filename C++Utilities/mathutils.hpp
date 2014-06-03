#ifndef _MATH_UTILS_HEADER_
#define _MATH_UTILS_HEADER_

class TDenseVector; 
class TDenseMatrix; 
class TIndex; 

TDenseMatrix kron(const TDenseMatrix &A, const TDenseMatrix &B); 
TDenseMatrix kronMat(const TDenseMatrix &A, const TDenseMatrix &B); 
TDenseVector kron(const TDenseVector &A, const TDenseMatrix &B); 
TDenseVector kronVect(const TDenseVector &A, const TDenseVector &B); 
TDenseVector linspace(double xmin, double xmax, int n); 
TIndex sort(const TDenseVector &x);  

class NumberWithOrder
{
private: 
	double number; 
	int order; 
public: 
	NumberWithOrder(double _number=0.0, int _order=0) :
	number(_number), order(_order) {} 
	NumberWithOrder(const NumberWithOrder &rhs) : 
	number(rhs.number), order(rhs.order) {}
	const NumberWithOrder & operator=(const NumberWithOrder &rhs) 
	{
		number = rhs.number; 
		order = rhs.order; 
		return *this; 
	}
	int Order() const { return order; }
	double Number() const { return number; }
}; 
#endif
