#include "TDenseMatrix4D.hpp"
#include "TDenseMatrix3D.hpp"
#include "dw_dense_matrix.hpp"
#include "dw_exception.hpp"

using namespace std; 

TDenseMatrix4D::TDenseMatrix4D(int _n1, int _n2, int _n3, int _n4, double _v) :
vector<TDenseMatrix3D>(_n1)
{
	for (int i1=0; i1<(int)(this->size()); i1++)
		this->operator[](i1) = TDenseMatrix3D(_n2,_n3,_n4, _v); 
}

TDenseMatrix4D::TDenseMatrix4D(const std::vector<TDenseMatrix3D> &_matrixArray) :
vector<TDenseMatrix3D>(_matrixArray.size())
{
	for (int i1=0; i1<(int)(this->size()); i1++)
		this->operator[](i1) = _matrixArray[i1]; 
}

TDenseMatrix4D::TDenseMatrix4D(const TDenseMatrix4D &right) :
vector<TDenseMatrix3D>(right.Size())
{
	for (int i1=0; i1<(int)(this->size()); i1++)
		this->operator[](i1) = right[i1]; 
}

TDenseMatrix4D::~TDenseMatrix4D() {}

int TDenseMatrix4D::Size(int d) const 
{
	if (d == 0) 
		return (int)this->size(); 
	else if (d == 1 && this->size()) 
		return this->operator[](0).Size(0); 
	else if (d == 2 && this->size()) 
		return this->operator[](0).Size(1); 
	else if (d == 3 && this->size()) 
		return this->operator[](0).Size(2); 
	else 
		throw dw_exception("TDenseMatrix4D::Size() must be along one of the 4 dimensions"); 
}

TDenseMatrix3D TDenseMatrix4D::operator()(int i) const {
	if (i >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::operator() index exceeds limit"); 
	return TDenseMatrix3D(this->operator[](i));
}

TDenseMatrix TDenseMatrix4D::operator()(int i, int j) const
{
	if (i >= (int)this->size())
                throw dw_exception("TDenseMatrix4D::operator() indices exceed limits");
	return TDenseMatrix(this->operator[](i).operator()(j)); 
}

TDenseVector TDenseMatrix4D::operator()(int i, int j, int k) const
{
	if (i >= (int)this->size())
                throw dw_exception("TDenseMatrix4D::operator() indices exceed limits");
        return TDenseVector(this->operator[](i).operator()(j,k));
}

double TDenseMatrix4D::operator()(int i1, int i2, int i3, int i4) const 
{
	if (i1 >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::operator() indices exceed limits");
        return this->operator[](i1).operator()(i2,i3,i4);
}

// Access on rhs (i1L:i1R,i2L:i2R,i3L:i3R,i4L:i4R)
TDenseMatrix4D TDenseMatrix4D:: operator()(const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
		if (n1[i1] >= (int)this->size())
			throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits"); 
	}
	TDenseMatrix4D matrix(n1.size, n2.size, n3.size, n4.size); 
	try {
		for (int i1=0; i1<n1.size; i1++) 
			matrix[i1] = this->operator[](n1[i1]).operator()(n2,n3,n4); 
	} 
	catch (...) {
		matrix.Clear();
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	return matrix; 
}

// Access on rhs (i1L:i1R,i2L:i2R,i3L:i3R,i4)
TDenseMatrix3D TDenseMatrix4D:: operator()(const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        }
	TDenseMatrix3D matrix(n1.size, n2.size, n3.size); 
	try {
		for (int i1=0; i1<n1.size; i1++)
			matrix[i1] = this->operator[](n1[i1]).operator()(n2,n3,n4); 
	}
	catch(...) {
		matrix.Clear(); 
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	return matrix; 
}

// Access on rhs (i1L:i1R,i2L:i2R,i3,i4L:i4R)
TDenseMatrix3D TDenseMatrix4D::operator()(const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        }
        TDenseMatrix3D matrix(n1.size, n2.size, n4.size);
	try {
                for (int i1=0; i1<n1.size; i1++)
                        matrix[i1] = this->operator[](n1[i1]).operator()(n2,n3,n4);
        }
        catch(...) {
                matrix.Clear();
                throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        }
        return matrix;
}

// Access on rhs (i1L:i1R,i2,i3L:i3R,i4L:i4R)
TDenseMatrix3D TDenseMatrix4D::operator()(const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4) const 
{
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        }
        TDenseMatrix3D matrix(n1.size, n3.size, n4.size);
        try {
                for (int i1=0; i1<n1.size; i1++)
                        matrix[i1] = this->operator[](n1[i1]).operator()(n2,n3,n4);
        }
        catch(...) {
                matrix.Clear();
                throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        }
        return matrix;
}

// Access on rhs (i1,i2L:iLR,i3L:i3R,i4L:i4R)
TDenseMatrix3D TDenseMatrix4D::operator()(int n1, TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	if (n1 >= (int)(this->size()) )
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	return this->operator[](n1).operator()(n2,n3,n4); 
}

// Access on rhs (n1L:n1R,n2L:n2R,n3,n4)
TDenseMatrix TDenseMatrix4D::operator()(const TIndex &n1, const TIndex &n2, int n3, int n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
		if (n1[i1] >= (int)this->size())
			throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	TDenseMatrix matrix(n1.size, n2.size); 
	try {
		for (int i1=0; i1<n1.size; i1++)
			matrix.InsertRowMatrix(i1,0,this->operator[](n1[i1]).operator()(n2,n3,n4)); 
	}
	catch(...) {
		matrix.Resize(0,0); 
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	return matrix; 
}

// Access on rhs (n1L:n1R,n2,n3L:n3R,n4)
TDenseMatrix TDenseMatrix4D::operator()(const TIndex &n1, int n2, const TIndex &n3, int n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
		if (n1[i1] >= (int)this->size())
			throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	TDenseMatrix matrix(n1.size, n3.size); 
	try {
		for (int i1=0; i1<n1.size; i1++)
			matrix.InsertRowMatrix(i1,0,this->operator[](n1[i1]).operator()(n2,n3,n4)); 
	}
	catch(...) {
		matrix.Resize(0,0); 
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	return matrix; 
}


// Access on rhs (n1L:n1R,n2,n3,n4L:n4R)
TDenseMatrix TDenseMatrix4D::operator()(const TIndex &n1, int n2, int n3, const TIndex &n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
		if (n1[i1] >= (int)this->size())
			throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	TDenseMatrix matrix(n1.size, n4.size); 
	try {
		for (int i1=0; i1<n1.size; i1++)
			matrix.InsertRowMatrix(i1,0,this->operator[](n1[i1]).operator()(n2,n3,n4)); 
	}
	catch(...) {
		matrix.Resize(0,0); 
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	return matrix; 
}

// Access on rhs (n1,n2L:n2R,n3L:n3R,n4)
TDenseMatrix TDenseMatrix4D::operator()(int n1, const TIndex &n2, const TIndex &n3, int n4) const 
{
	if (n1 >= this->size())
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	return this->operator[](n1).operator()(n2,n3,n4); 
}

// Access on rhs (n1,n2L:n2R,n3,n4L:n4R)
TDenseMatrix TDenseMatrix4D::operator()(int n1, const TIndex &n2, int n3, const TIndex &n4) const 
{
	if (n1 >= this->size())
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	return this->operator[](n1).operator()(n2,n3,n4); 
}

// Access on rhs (n1,n2,n3L:n3R,n4L:n4R)
TDenseMatrix TDenseMatrix4D::operator()(int n1, int n2, const TIndex &n3, const TIndex &n4) const 
{
	if (n1 >= this->size())
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	return this->operator[](n1).operator()(n2,n3,n4); 
}

// Access on rhs (n1L:n1R,n2,n3,n4)
TDenseVector TDenseMatrix4D::operator()(const TIndex &n1,int n2, int n3, int n4) const
{
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size() )
                        throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        }
	TDenseVector vector(n1.size); 
	try {
		for (int i1=0; i1<n1.size; i1++)
			vector[i1] = this->operator[](n1[i1]).operator()(n2,n3,n4);
	} 
	catch(...)
	{
		vector.Resize(0); 
		throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
	}
	return vector; 
}

//Access on rhs (n1,n2L:n2R, n3, n4)
TDenseVector TDenseMatrix4D::operator()(int n1, const TIndex &n2, int n3, int n4) const
{
	if (n1 >= this->size())
                throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        return this->operator[](n1).operator()(n2,n3,n4);
}

//Access on rhs (n1,n2, n3L:n3R, n4)
TDenseVector TDenseMatrix4D::operator()(int n1, int n2, const TIndex &n3, int n4) const
{
	if (n1 >= this->size())
                throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        return this->operator[](n1).operator()(n2,n3,n4);
}

//Access on rhs (n1,n2, n3, n4L:n4R)
TDenseVector TDenseMatrix4D::operator()(int n1, int n2, int n3, const TIndex &n4) const
{
	if (n1 >= this->size())
                throw dw_exception("TDenseMatrix4D::operator() : indices exceed limits");
        return this->operator[](n1).operator()(n2,n3,n4);
}

// Set values (n1,n2,n3,n4)=v
void TDenseMatrix4D::Set(double v,int n1, int n2, int n3, int n4)
{
	if (n1 >= this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits"); 
	this->operator[](n1).Set(v,n2,n3,n4); 
}

// Set value (n1,n2,n3,:) = v
void TDenseMatrix4D::Set(const TDenseVector &v, int n1, int n2, int n3)
{
	if (n1 >= this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
	this->operator[](n1).Set(v,n2,n3);
}

// Set value (n1,n2,:,:) = v
void TDenseMatrix4D::Set(const TDenseMatrix &v, int n1, int n2)
{
	if (n1 >= this->size())
                throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        this->operator[](n1).Set(v,n2);
}

// Set value (n1,:,:,:) = v
void TDenseMatrix4D::Set(const TDenseMatrix3D &v, int n1)
{
	if (n1 >= this->size())
                throw dw_exception("TDenseMatrix4D::Set() : index exceeds limit");
	this->operator[](n1) = v;  
}

// Set value (n1L,n2R,n2L:n2R,n3L:n3R,n4L:n4R) = v
void TDenseMatrix4D::Set(const TDenseMatrix4D &v, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4)
{
	if (v.Size() != n1.size)
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match"); 
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        }
	try {
		for (int i1=0; i1<n1.size; i1++)
			this->operator[](n1[i1]).Set(v(i1),n2,n3,n4);
	}
	catch(...) {
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits"); 
	} 
}

// Set value (n1L:n1R,n2L:n2R,n3L:n3R,n4) = v
void TDenseMatrix4D::Set(const TDenseMatrix3D &v, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4)
{
	if (v.Size() != n1.size)
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match");
	for (int i1=0; i1<n1.size; i1++) {
		if (n1[i1] >= (int)this->size())
			throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
	}
	try {
		for (int i1=0; i1<n1.size; i1++)
			this->operator[](n1[i1]).Set(v(i1),n2,n3,n4); 
	}
	catch(...) {
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1L:n1R,n2L:n2R,n3,n4L:n4R) = v
void TDenseMatrix4D::Set(const TDenseMatrix3D &v, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4)
{
	if (v.Size() != n1.size)
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match");
        for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        }
        try {
                for (int i1=0; i1<n1.size; i1++)
                        this->operator[](n1[i1]).Set(v(i1),n2,n3,n4); 
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
        }
}

// Set value (n1L:n1R,n2,n3L:n3R,n4L:n4R) = v
void TDenseMatrix4D::Set(const TDenseMatrix3D &v, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4)
{
	if (v.Size() != n1.size)
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match");
        for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        }
        try {
                for (int i1=0; i1<n1.size; i1++)
                        this->operator[](n1[i1]).Set(v(i1),n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
        }
}

// Set value (n1, n2L:n2R,n3L:n3R,n4L:n4R) = v
void TDenseMatrix4D::Set(const TDenseMatrix3D &v, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4)
{
	if (n1 >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
	try {
		this->operator[](n1).Set(v,n2,n3,n4);
	}
	catch(...) {
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1L:n1R, n2L:n2R,n3,n4) =v
void TDenseMatrix4D::Set(const TDenseMatrix &v, const TIndex &n1, const TIndex &n2, int n3, int n4)
{
	if (v.rows != n1.size)
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match");
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        }
	try {
		for (int i1=0; i1<n1.size; i1++)
			this->operator[](n1[i1]).Set(RowVector(v,i1),n2,n3,n4); 
	}
	catch(...) {
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1L:n1R, n2, n3L:n3R, n4) =v
void TDenseMatrix4D::Set(const TDenseMatrix &v, const TIndex &n1, int n2, const TIndex &n3, int n4)
{
	if (v.rows != n1.size)
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match");
        for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
	}
        try {
                for (int i1=0; i1<n1.size; i1++)
                        this->operator[](n1[i1]).Set(RowVector(v,i1),n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1L:n1R, n2, n3, n4L:n4R) =v
void TDenseMatrix4D::Set(const TDenseMatrix &v, const TIndex &n1, int n2, int n3, const TIndex &n4)
{
	if (v.rows != n1.size)
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match");
        for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        }
        try {
                for (int i1=0; i1<n1.size; i1++)
                        this->operator[](n1[i1]).Set(RowVector(v,i1),n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1,n2L:n2R,n3L:n3R,n4)=v;
void TDenseMatrix4D::Set(const TDenseMatrix &v, int n1, const TIndex &n2, const TIndex &n3, int n4)
{
	if (n1 >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
	try{
		this->operator[](n1).Set(v,n2,n3,n4); 
	}
	catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1,n2L:n2R,n3,n4L:n4R)=v
void TDenseMatrix4D::Set(const TDenseMatrix &v, int n1, const TIndex &n2, int n3, const TIndex &n4)
{
        if (n1 >= (int)this->size())
                throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        try{
                this->operator[](n1).Set(v,n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1,n2,n3L:n3R,n4L:n4R)=v
void TDenseMatrix4D::Set(const TDenseMatrix &v, int n1, int n2, const TIndex &n3,  const TIndex &n4)
{
        if (n1 >= (int)this->size())
                throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        try{
                this->operator[](n1).Set(v,n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
	}
}

// Set value (n1L:n1R,n2,n3,n4) = v
void TDenseMatrix4D::Set(const TDenseVector &v, const TIndex &n1, int n2, int n3, int n4)
{
	if (v.dim != n1.size)
		throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match"); 
	for (int i1=0; i1<n1.size; i1++) {
                if (n1[i1] >= (int)this->size())
                        throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        }
        try {
                for (int i1=0; i1<n1.size; i1++)
                        this->operator[](n1[i1]).Set(v[i1],n2,n3,n4);
        }
 	catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
        }
}

// Set value (n1,n2L:n2R,n3,n4) = v
void TDenseMatrix4D::Set(const TDenseVector &v, int n1, const TIndex &n2, int n3, int n4)
{
	if (n1 >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        try{
                this->operator[](n1).Set(v,n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
        }
}

// Set value (n1,n2, n3L:n3R,n4) = v
void TDenseMatrix4D::Set(const TDenseVector &v, int n1, int n2, const TIndex &n3, int n4)
{
	if (n1 >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        try{
                this->operator[](n1).Set(v,n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
        }
}

// Set value (n1,n2,n3,n4L:n4R) = v
void TDenseMatrix4D::Set(const TDenseVector &v, int n1, int n2, int n3, const TIndex &n4)
{
	if (n1 >= (int)this->size())
		throw dw_exception("TDenseMatrix4D::Set() : indices exceed limits");
        try{
                this->operator[](n1).Set(v,n2,n3,n4);
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::Set() : dimensions of value and indices do not match or indices exceed limits");
        }
}

// Operators
const TDenseMatrix4D & TDenseMatrix4D::operator=(const TDenseMatrix4D &right)
{
	this->resize(right.size()); 
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i) = right[i]; 
	return *this; 
}

TDenseMatrix4D TDenseMatrix4D::operator+(double v) const
{
	TDenseMatrix4D result(this->size()); 
	for (int i=0; i<(int)result.size(); i++)
		result[i] = this->operator[](i)+v; 
	return result; 
}

TDenseMatrix4D TDenseMatrix4D::operator+(const TDenseMatrix4D &right) const
{
	if (this->size() != right.size())
		throw dw_exception("TDenseMatrix4D::operator+ : dimension does not match");
	TDenseMatrix4D result(this->size()); 
	try {
		for (int i=0; i<(int)(result.size()); i++)
			result[i] = this->operator[](i)+right[i]; 
	}
	catch(...) {
		throw dw_exception("TDenseMatrix4D::operator+ dimension does not match");
	}
	return result; 
}

TDenseMatrix4D TDenseMatrix4D::operator*(double v) const
{
        TDenseMatrix4D result(this->size());
        for (int i=0; i<(int)result.size(); i++)
                result[i] = this->operator[](i)*v;
        return result; 
}

TDenseMatrix4D TDenseMatrix4D::operator*(const TDenseMatrix4D &right) const
{
        if (this->size() != right.size())
                throw dw_exception("TDenseMatrix4D::operator* : dimension does not match");
        TDenseMatrix4D result(this->size()); 
        try {
                for (int i=0; i<(int)(result.size()); i++)
                        result[i] = this->operator[](i)*right[i]; 
        }
        catch(...) {
                throw dw_exception("TDenseMatrix4D::operator* dimension does not match");
        }
        return result; 
}

// Sum
TDenseMatrix3D TDenseMatrix4D::sum(int d) const
{
	if (!this->size())
		return TDenseMatrix3D(0,0,0,0.0); 

	TIndex n1(0,(int)(this->size()-1)), n2(0, this->operator()(0).Size()-1), n3(0, this->operator()(0,0).rows-1), n4(0, this->operator()(0,0).cols-1); 
	if (d == 0) {
		TDenseMatrix3D result(n2.size,n3.size,n4.size,0.0); 
		for (int i1=0; i1<n1.size; i1++)
			result = result + this->operator()(n1[i1],n2,n3,n4); 
		return result; 	
	}
	else if (d == 1) {
		TDenseMatrix3D result(n1.size,n3.size,n4.size,0.0); 
		for (int i2=0; i2<n2.size; i2++)
			result = result + this->operator()(n1,n2[i2],n3,n4); 
		return result; 
	}
	else if (d == 2) {
		TDenseMatrix3D result(n1.size,n2.size,n4.size,0.0);
		for (int i3=0; i3<n3.size; i3++)
			result = result + this->operator()(n1,n2,n3[i3],n4); 
		return result; 
	}
	else if (d == 3) {
                TDenseMatrix3D result(n1.size,n2.size,n3.size,0.0); 
                for (int i4=0; i4<n4.size; i4++)
                        result = result + this->operator()(n1,n2,n3,n4[i4]); 
                return result;
	}
	else 
		throw dw_exception("TDenseMatrix4D::sum() : must along one of the 4 dimensions"); 
}

TDenseMatrix TDenseMatrix4D::sum(int d1, int d2) const 
{
	if (!this->size())
		return TDenseMatrix(0,0,0.0); 

	TIndex n1(0,(int)(this->size()-1)), n2(0, this->operator()(0).Size()-1), n3(0, this->operator()(0,0).rows-1), n4(0, this->operator()(0,0).cols-1);

	if (d1 == 0 && d2 == 1 || d1 == 1 && d2 == 0 ) {
		TDenseMatrix result(n3.size,n4.size,0.0); 
		for (int i1=0; i1<n1.size; i1++)
			for (int i2=0; i2<n2.size; i2++)
				result = result + this->operator()(n1[i1],n2[i2],n3,n4); 
		return result; 
	}
	else if (d1 == 0 && d2 == 2 || d1 == 2 && d2 == 0) {
		TDenseMatrix result(n2.size,n4.size,0.0); 
		for (int i1=0; i1<n1.size; i1++)
			for (int i3=0; i3<n3.size; i3++)
				result = result + this->operator()(n1[i1],n2,n3[i3],n4); 
		return result; 
	}
	else if (d1 == 0 && d2 == 3 || d1 == 3 && d2 == 0) {
                TDenseMatrix result(n2.size,n3.size,0.0); 
                for (int i1=0; i1<n1.size; i1++)
                        for (int i4=0; i4<n4.size; i4++)
                                result = result + this->operator()(n1[i1],n2,n3,n4[i4]); 
                return result;
	}
	else if (d1 == 1 && d2 == 2 || d1 == 2 && d2 == 1) {
		TDenseMatrix result(n1.size,n4.size,0.0); 
		for (int i2=0; i2<n2.size; i2++)
			for (int i3=0; i3<n3.size; i3++)
				result = result + this->operator()(n1,n2[i2],n3[i3],n4); 
		return result; 
	}
	else if (d1 == 1 && d2 == 3 || d1 == 3 && d2 == 1) {
		TDenseMatrix result(n1.size,n3.size,0.0); 
		for (int i2=0; i2<n2.size; i2++)
			for (int i4=0; i4<n4.size; i4++)
				result = result + this->operator()(n1,n2[i2],n3,n4[i4]); 
		return result; 
	}
	else if (d1 == 2 && d2 == 3 || d1 == 3 && d2 == 2) {
		TDenseMatrix result(n1.size,n2.size,0.0); 
		for (int i3=0; i3<n3.size; i3++)
			for (int i4=0; i4<n4.size; i4++)
				result = result + this->operator()(n1,n2,n3[i3],n4[i4]); 
		return result;
	}
	else 
		throw dw_exception("TDenseMatrix4D::sum() : must along two of the 4 dimensions");
}

TDenseVector TDenseMatrix4D::sum(int d1, int d2, int d3) const
{
	if (!this->size())
		return TDenseVector(0,0.0); 
	if (d1 == d2 || d1 == d3 || d2 == d3)
		throw dw_exception("TDenseMatrix4D::sum() : mulst along three of the 4 dimensions"); 

	TIndex n1(0,(int)(this->size()-1)), n2(0, this->operator()(0).Size()-1), n3(0, this->operator()(0,0).rows-1), n4(0, this->operator()(0,0).cols-1);
	if (d1 >= 0 && d1 <= 2 && d2 >= 0 && d2 <= 2 && d3 >= 0 && d3 <= 2) // d1,d2,d3 = (0,1,2) or any combination
	{
		TDenseVector result(n4.size,0.0); 
		for (int i1=0; i1<n1.size; i1++)
			for (int i2=0; i2<n2.size; i2++)
				for (int i3=0; i3<n3.size; i3++)
					result = result + this->operator()(n1[i1],n2[i2],n3[i3],n4); 
		return result; 
	}
	else if ( d1 != 2 && d1 >= 0 && d1 <= 3 && d2 != 2 && d2 >= 0 && d2 <= 3 && d3 != 2 && d3 >= 0 && d3 <= 3 ) // d1, d2, d3 = (0,1,3) or any combination
	{
		TDenseVector result(n3.size, 0.0); 
		for (int i1=0; i1<n1.size; i1++)
                        for (int i2=0; i2<n2.size; i2++)
                                for (int i4=0; i4<n4.size; i4++)
                                        result = result + this->operator()(n1[i1],n2[i2],n3,n4[i4]); 
                return result;
	}
	else if (d1 != 1 && d1 >= 0 && d1 <= 3 && d2 != 1 && d2 >= 0 && d2 <= 3 && d3 != 1 && d3 >= 0 && d3 <= 3 ) // d1, d2, d3 = (0, 2, 3) or any combination
	{
		TDenseVector result(n2.size,0.0); 
		for (int i1=0; i1<n1.size; i1++)
			for (int i3=0; i3<n3.size; i3++)
				for (int i4=0; i4<n4.size; i4++)
					result = result + this->operator()(n1[i1],n2,n3[i3],n4[i4]); 
		return result; 
	}
	else if (d1 >= 1 && d1 <= 3 && d2 >= 1 && d2 <= 3 && d3 >= 1 && d3 <= 3 ) // d1, d2, d3 = (1, 2, 3) or any combination
	{
		TDenseVector result(n1.size,0.0); 
		for (int i2=0; i2<n2.size; i2++)
                        for (int i3=0; i3<n3.size; i3++)
                                for (int i4=0; i4<n4.size; i4++)
                                        result = result + this->operator()(n1,n2[i2],n3[i3],n4[i4
]); 
                return result;
	}
	else 
		throw dw_exception("TDenseMatrix4D::sum() : mulst along three of the 4 dimensions");
}

double TDenseMatrix4D::sum() const
{
	double result=0.0; 
	for (int i=0; i<(int)(this->size()); i++)
		result += this->operator[](i).sum(); 
	return result; 
}

void TDenseMatrix4D::Clear()
{
	for (int i=0; i<(int)(this->size()); i++)
		this->operator[](i).Clear(); 
	this->clear(); 
}

void TDenseMatrix4D::Resize(int n1, int n2, int n3, int n4)
{
	this->resize(n1); 
	for (int i=0; i<n1; i++)
		this->operator[](i).Resize(n2,n3,n4); 
}
