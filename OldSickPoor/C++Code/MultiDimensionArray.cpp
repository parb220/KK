#include "dw_dense_matrix.hpp"
#include "dw_exception.hpp"
#include "MultiDimensionArray.hpp"

using namespace std; 

// Construction 
TDenseMatrix3D::TDenseMatrix3D(int _d1, int _d2, int _d3) :
vector<TDenseMatrix>(_d1)
{
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i) = TDenseMatrix(_d2, _d3, 0.0); 
}

TDenseMatrix3D::TDenseMatrix3D(const std::vector<TDenseMatrix> & _matrixArray) : 
vector<TDenseMatrix>(_matrixArray.size())
{
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i) = _matrixArray[i]; 
}

TDenseMatrix3D::TDenseMatrix3D(const TDenseMatrix3D & right) : 
vector<TDenseMatrix>(right.size())
{
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i).Insert(0,0,right[i]); 
}

// Destruction
TDenseMatrix3D::~TDenseMatrix3D() {}

// Access
int TDenseMatrix3D::Size() const { return (int)size(); }

// Access used on rhs (i,:,:)
TDenseMatrix TDenseMatrix3D::operator()(int i) const {
	if (i >= (int)size())
		throw dw_exception("TDenseMatrix3D::operator() index exceeds limit");
	return TDenseMatrix(this->operator[](i)); 
}

// Access used on rhs (i,j,:)
TDenseVector TDenseMatrix3D::operator()(int i, int j) const {
	if (i >= (int)(size()) || j >= this->operator[](i).rows)
		throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
	return TDenseVector(RowVector(this->operator[](i),j)); 
}

// Access used on rhs (i,j,k)
double TDenseMatrix3D::operator()(int i, int j, int k) const {
	if (i >= (int)(size()) || j >= this->operator[](i).rows || k >= this->operator[](i).cols) 
		throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
	return this->operator[](i).operator()(j,k); 
}

// Access used on rhs (i1:i2, j1:j2, k1:k2)
TDenseMatrix3D TDenseMatrix3D::operator()(const TIndex &i, const TIndex &j, const TIndex &k) const
{
	for (int ii=0; ii<i.size; ii++) {
		if (i[ii] >= size()) 
			throw dw_exception("TDenseMatrix3D::operator() indices exceed limits"); 
	}
	for (int jj=0; jj<j.size; jj++) {
		if (size() && j[jj] >= this->operator[](0).rows)
			throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
	}
	for (int kk=0; kk<k.size; kk++) {
		if (size() && k[kk] >= this->operator[](0).cols)
			throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");	
	}

	TDenseMatrix3D x(i.size, j.size, k.size); 
	for (int ii=0; ii<i.size; ii++) {
		x[ii].Insert(0,0,this->operator[](i[ii]).SubMatrix(j,k)); 
	}
	return x; 
}

// Access used on rhs (i, j1:j2, k1:k2)
TDenseMatrix TDenseMatrix3D::operator()(int i, const TIndex &j, const TIndex &k) const
{
	if (i >= (int)size())
		throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");	
	for (int jj=0; jj<j.size; jj++) {
                if (j[jj] >= this->operator[](i).rows)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
        for (int kk=0; kk<k.size; kk++) {
                if (k[kk] >= this->operator[](i).cols)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	return TDenseMatrix(this->operator[](i).SubMatrix(j,k)); 
}

// Access used on rhs (i1:i2, j, k1:k2)
TDenseMatrix TDenseMatrix3D::operator()(const TIndex &i, int j, const TIndex &k) const
{
	for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= size() || j >= this->operator[](i[ii]).rows)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	for (int kk=0; kk<k.size; kk++) {
                if (size() && k[kk] >= this->operator[](0).cols)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	TDenseMatrix x(i.size,k.size,0.0); 
	for (int ii=0; ii<i.size; ii++)
		x.InsertRowMatrix(ii,0,RowVector(this->operator[](i[ii]),j,k)); 
	return x; 
}

// Access used on rhs (i1:i2, j1:j2, k)
TDenseMatrix TDenseMatrix3D::operator()(const TIndex &i, const TIndex &j, int k) const
{
	for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= size() || k >= this->operator[](i[ii]).cols)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	for (int jj=0; jj<j.size; jj++) {
                if (size() && j[jj] >= this->operator[](0).rows)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	TDenseMatrix x(i.size,j.size,0.0); 
	for (int ii=0; ii<i.size; ii++)
		x.InsertRowMatrix(ii,0,ColumnVector(this->operator[](i[ii]),k,j)); 
	return x; 
}

// Access used on rhs (i, j1:j2, k)
TDenseVector TDenseMatrix3D::operator()(int i, const TIndex &j, int k) const
{
	if (i>=size() || k >= this->operator[](i).cols)
		throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
	for (int jj=0; jj<j.size; jj++) {
                if (size() && j[jj] >= this->operator[](0).rows)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	return ColumnVector(this->operator[](i),k,j); 
}

// Access used on rhs (i, j, k1:k2)
TDenseVector TDenseMatrix3D::operator()(int i, int j, const TIndex &k) const
{
	if (i>=size() || j >= this->operator[](i).rows)
		throw dw_exception("TDenseMatrix3D::operator() indices exceed limits"); 
	for (int kk=0; kk<k.size; kk++) {
                if (size() && k[kk] >= this->operator[](0).cols)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }
	return RowVector(this->operator[](i),j,k); 
}

// Access used on rhs (i1:i2, j, k)
TDenseVector TDenseMatrix3D::operator()(const TIndex &i, int j, int k) const
{
	for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= size() || j >= this->operator[](i[ii]).rows || k >= this->operator[](i[ii]).cols)
                        throw dw_exception("TDenseMatrix3D::operator() indices exceed limits");
        }

	TDenseVector x(i.size,0.0); 
	for (int ii=0; ii<i.size; ii++)
		x[ii] = this->operator()(i[ii],j,k); 
}

// Set value (i,:,:) = v
void TDenseMatrix3D::Set(const TDenseMatrix &v, int i) {
	if (i >= (int)size() || v.rows != this->operator[](i).rows || v.cols != this->operator[](i).cols)
		throw dw_exception("TDenseMatrix3D::Set() index exceeds limit or matrix dimensions do not match");
	this->operator[](i).Insert(0,0,v); 
}

// Set value(i,j,:) = v
void TDenseMatrix3D::Set(const TDenseVector &v, int i, int j) {
	if (i >= (int)size() || j >= this->operator[](i).rows || v.dim != this->operator[](i).cols)
		throw dw_exception("TDenseMatrix3D::Set() indices exceed limits or vector dimensions do not match");
	this->operator[](i).InsertRowMatrix(j,0,v); 
}

// Set value (i,j,k) = v
void TDenseMatrix3D::Set(double v, int i, int j, int k) {
	if (i >= (int)size() || j >= this->operator[](i).rows || k >= this->operator[](i).cols)
		throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
	this->operator[](i).operator()(j,k) = v;  
} 

// Set value (i1:i2, j1:j2, k1:k2) 
void TDenseMatrix3D::Set(const TDenseMatrix3D &v, const TIndex &i, const TIndex &j, const TIndex &k)
{
	if ((int)v.size() != i.size || (v.size() && (v[0].rows != j.size || v[0].cols != k.size) ))
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match"); 
	for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= size())
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
        for (int jj=0; jj<j.size; jj++) {
                if (size() && j[jj] >= this->operator[](0).rows)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
        for (int kk=0; kk<k.size; kk++) {
                if (size() && k[kk] >= this->operator[](0).cols)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
	for (int ii=0; ii<i.size; ii++)
		this->operator[](i[ii]).Insert(j,k,v[ii]); 	
}

// Set value (i,j1:j2,k1:k2)
void TDenseMatrix3D::Set(const TDenseMatrix &v, int i, const TIndex &j, const TIndex &k)
{
	if (v.rows != j.size || v.cols != k.size)
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match");
	if (i >= (int)size())
		throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
	for (int jj=0; jj<j.size; jj++) {
                if (j[jj] >= this->operator[](i).rows)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
        for (int kk=0; kk<k.size; kk++) {
                if (k[kk] >= this->operator[](i).cols)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
	this->operator[](i).Insert(j,k,v); 
}

// Set value (i1:i2, j, k1:k2)
void TDenseMatrix3D::Set(const TDenseMatrix &v, const TIndex &i, int j, const TIndex &k)
{
	if (v.rows != i.size || v.cols != k.size)
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match");
	for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= size())
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
	for (int kk=0; kk<k.size; kk++) {
                if (size() && k[kk] >= this->operator[](0).cols)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
	for (int ii=0; ii<i.size; ii++) 
		this->operator[](i[ii]).InsertRowMatrix(j,k,RowVector(v,ii)); 
}

// Set value (i1:i2, j1:j2, k)
void TDenseMatrix3D::Set(const TDenseMatrix &v, const TIndex &i, const TIndex &j, int k)
{
	if (v.rows != i.size || v.cols != j.size)
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match");
        for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= (int)size())
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
	for (int jj=0; jj<j.size; jj++) {
                if (size() && j[jj] >= this->operator[](0).rows)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
        }
	for (int ii=0; ii<i.size; ii++)
		this->operator[](i[ii]).InsertColumnMatrix(j,k,RowVector(v,ii)); 
}

// Set value (i, j, k1:k2)
void TDenseMatrix3D::Set(const TDenseVector &v, int i, int j, const TIndex &k)
{
	if (v.dim != k.size)
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match");
	if (i >= (int)size() || j >= this->operator[](i).rows)
		throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
	this->operator[](i).InsertRowMatrix(j,k,v); 
}

// Set value (i, j1:j2, k)
void TDenseMatrix3D::Set(const TDenseVector &v, int i, const TIndex &j, int k)
{
	if (v.dim != j.size)
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match");
        if (i >= (int)size() || k >= this->operator[](i).cols)
		throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
	this->operator[](i).InsertColumnMatrix(j,k,v); 
}

// Set value (i1:i2, j, k)
void TDenseMatrix3D::Set(const TDenseVector &v, const TIndex &i, int j, int k)
{
	if (v.dim != i.size)
		throw dw_exception("TDenseMatrix3D::Set() value and index dimensions do not match");
	for (int ii=0; ii<i.size; ii++) {
                if (i[ii] >= (int)size() || j >= this->operator[](i[ii]).rows || k >= this->operator[](i[ii]).cols)
                        throw dw_exception("TDenseMatrix3D::Set() indices exceed limits");
		this->operator[](i[ii]).operator()(j,k) = v[ii]; 
        }
}

const TDenseMatrix3D & TDenseMatrix3D::operator=(const TDenseMatrix3D &right)
{
	this->resize(right.size()); 
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i).CopyContent(right[i]); 
	return *this; 
}

TDenseMatrix3D TDenseMatrix3D:: operator+(double v) const 
{
	TDenseMatrix3D add_result(*this); 
	for (int i=0; i<(int)(add_result.size()); i++)
		add_result[i] += v*Ones(this->operator[](i).rows, this->operator[](i).cols); 
	return add_result; 
}

TDenseMatrix3D TDenseMatrix3D::operator+(const TDenseMatrix3D &right) const
{
	if(this->size()!=right.size())
		throw dw_exception("TDenseMatrix3D::operator+ : dimension does not match"); 

	TDenseMatrix3D add_result(*this); 
	try {
		for (int i=0; i<(int)(add_result.size()); i++)
			add_result[i] += right[i]; 
	}
	catch(...) {
		throw dw_exception("TDenseMatrix3D::operator+ dimension does not match");
	}	
	return add_result; 
} 

TDenseMatrix3D TDenseMatrix3D::operator*(double v) const
{
	TDenseMatrix3D multiply_result(*this); 
	for (int i=0; i<(int)(multiply_result.size()); i++)
		multiply_result[i] = multiply_result[i] * v; 
	return multiply_result;  
}

TDenseMatrix3D TDenseMatrix3D::operator*(const TDenseMatrix3D &right) const
{
	if(this->size()!=right.size())
		throw dw_exception("TDenseMatrix3D::operator* : dimension does not match"); 
	TDenseMatrix3D multiply_result(*this); 
	for (int i=0; i<(int)(multiply_result.size()); i++)
	{
		if (this->operator[](i).rows != right[i].rows || this->operator[](i).cols != right[i].cols)
			throw dw_exception("TDenseMatrix3D::operator* : dimension does not match");
		for (int j=0; j<this->operator[](i).rows; j++)
			for (int k=0; k<this->operator[](i).cols; k++)
				multiply_result.Set(multiply_result(i,j,k)* right(i,j,k), i, j, k); 	
	}
	return multiply_result; 
}

TDenseMatrix TDenseMatrix3D::sum(int d) const
{
	TDenseMatrix sum_result; 
	switch(d)
	{
		case 0: {
			sum_result.CopyContent(this->operator[](0)); 
			for (int i=1; i<(int)this->size(); i++)
				sum_result += this->operator[](i); 
		} break; 
		case 1: {
			sum_result.Resize(this->size(), this->operator[](0).cols); 
			for (int i=0; i<(int)this->size(); i++) {
				for (int k=0; k<this->operator[](0).cols; k++) {
					sum_result(i,k) = 0.0; 
					for (int j=0; j<this->operator[](0).rows; j++) 
						sum_result(i,k) += this->operator()(i,j,k);
				}
			}
		} break; 
		case 2:{
			sum_result.Resize(this->size(), this->operator[](0).rows);
			for (int i=0; i<(int)this->size(); i++) {
                                for (int j=0; j<this->operator[](0).rows; j++) {
                                        sum_result(i,j) = 0.0;
                                        for (int k=0; k<this->operator[](0).cols; k++) 
                                                sum_result(i,j) += this->operator()(i,j,k);
                                }
                        }
		} break; 
		default:
			throw dw_exception("TDenseMatrix3D::sum() : must along one of the 3 dimensions"); 
	}
	return sum_result; 
}

TDenseVector TDenseMatrix3D::sum(int i, int j) const
{
	TDenseVector sum_result; 
	if (i == 0 && j == 1)
	{
		sum_result.Resize(this->operator[](0).cols); 
		for (int k=0; k<this->operator[](0).cols; k++)
		{
			sum_result[k] = 0.0; 
			for (int i=0; i<(int)(this->size()); i++)
				for (int j=0; j<this->operator[](0).rows; j++)
					sum_result[k] += this->operator()(i,j,k); 
		}
	}
	else if (i == 0 && j == 2)
	{
		sum_result.Resize(this->operator[](0).rows); 
		for (int j=0; j<this->operator[](0).rows; j++)
		{
			sum_result[j] = 0.0; 
			for (int i=0; i<(int)(this->size()); i++)
				for (int k=0; k<this->operator()(i).cols; k++)
					sum_result[j] += this->operator()(i,j,k); 
		}	
	}
	else if (i == 1 && j == 2)
	{
		sum_result.Resize(this->size());
		for (int i=0; i<this->size(); i++)
		{
			for (int j=0; j<this->operator[](i).rows; j++)
				for (int k=0; k<this->operator[](i).cols; k++)
					sum_result[i] += this->operator()(i,j,k); 
		}
	}
	else 
		throw dw_exception("TDenseMatrix3D::sum() : must along one of the 3 dimensions");
	return sum_result; 
}

double TDenseMatrix3D::sum()const
{
	double sum_result = 0; 
	for (int i=0; i<this->size(); i++)
		for (int j=0; j<this->operator[](i).rows; j++)
			for (int k=0; k<this->operator[](k).cols; k++)
				sum_result += this->operator()(i,j,k);
	return sum_result; 
}

TDenseMatrix3D & TDenseMatrix3D::Clear()
{
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i).Resize(0,0); 
	clear(); 
	return *this; 
}

TDenseMatrix3D & TDenseMatrix3D::Resize(int i, int j, int k)
{
	if (this->size() && this->operator[](0).rows == j && this->operator[](0).cols == k)
		resize(i); 
	else 
	{
		Clear(); 
		resize(i); 
		for (int ii=0; ii<i; ii++)
			this->operator[](ii) = TDenseMatrix(j,k, 0.0); 
	}
	return *this; 
}
