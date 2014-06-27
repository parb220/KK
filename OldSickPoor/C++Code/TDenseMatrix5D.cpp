#include <vector>
#include "dw_dense_matrix.hpp"
#include "dw_exception.hpp"

using namespace std; 

TDenseMatrix5D::TDenseMatrix5D(int _n0, int _n1, int _n2, int _n3, int _n4, double _v) :
vector<TDenseMatrix4D>(_n0)
{
	for (int i=0; i<_n0; i++)
		this->operator[](i) = TDenseMatrix4D(_n1,_n2,_n3,_n4,_v); 
}

TDenseMatrix5D::TDenseMatrix5D(const std::vector<TDenseMatrix4D> &_matrixArray) :
vector<TDenseMatrix4D>(_matrixArray.size())
{
	for (int i=0; i<(int)this->size(); i++)
		this->operator[](i) = _matrixArray[i]; 
}

TDenseMatrix5D::TDenseMatrix5D(const TDenseMatrix5D &right) : 
vector<TDenseMatrix4D>(right.Size())
{
	for (int i=0; i<(int)this->size(); i++)
		this->operator[](i) = right[i]; 
}

TDenseMatrix5D::~TDenseMatrix5D() {}

int TDenseMatrix5D::Size(int d) const
{
	if (d == 0 )
		return (int)this->size(); 
	else if (d >= 1 && d <= 4 && this->size())
		return this->operator[](0).Size(d-1); 
	else 
		throw dw_exception("TDenseMatrix5D::Size() must be along one of the 5 dimensions"); 
}

// RHS Access
TDenseMatrix4D TDenseMatrix5D operator()(int i) const
{
	if (i< 0 || i >= (int) this->size())
		throw dw_exception("TDenseMatrix5D::operator() index exceeds limit"); 
	return TDenseMatrix4D(this->operator[](i)); 
}

TDenseMatrix3D TDenseMatrix5D:: operator()(int i, int j) const
{
	if (i< 0 || i >= (int) this->size())
                throw dw_exception("TDenseMatrix5D::operator() index exceeds limit");
	return TDenseMatrix3D(this->operator[](i).operator()(j)); 
}

TDenseMatrix TDenseMatrix5D::operator()(int i, int j, int k) const
{
	if (i<0 || i>= (int)this->size())
		throw dw_exception("TDenseMatrix5D::operator() index exceeds limit");
	return TDenseMatrix(this->operator[](i).operaotr()(j,k)); 
}

TDenseVector TDenseMatrix5D::operator()(int i0, int i1, int i2, int i3) const
{
	if (i0<0 || i0>=(int)this->size())
		 throw dw_exception("TDenseMatrix5D::operator() index exceeds limit");
	return TDenseVector(this->operator[](i0).operator()(i1,i2,i3)); 
}

double TDenseMatrix5D::operator()(int i0, int i1, int i2, int i3, int i4) const
{
	if (i0<0 || i0>=(int)this->size())
		throw dw_exception("TDenseMatrix5D::operator() index exceeds limit");
	return this->operator[](i0).operator(i1,i2,i3,i4); 
}
