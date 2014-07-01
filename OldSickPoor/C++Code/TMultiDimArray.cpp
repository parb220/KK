#include <vector>
#include "dw_dense_matrix.hpp"
#include "dw_exception.hpp"
#include "TMultiDimArray.hpp"

using namespace std; 

TMultiDimArray::TMultiDimArray(int _n, double _v) :
TDenseVector(_n,_v) 
{	
	size_along_dim.resize(1); 
	size_along_dim[0] = _n; 
}

TMultiDimArray::TMultiDimArray(int _n) :
TDenseVector(_n)
{
	size_along_dim.resize(1); 
	size_along_dim[0] = _n; 
}

TMultiDimArray::TMultiDimArray(int _n, const TDenseVector &right) :
TDenseVector(_n),
size_along_dim(1,_n)
{
	if (_n < right.dim)
		this->Insert(0,right.SubVector(0,_n-1)); 
	else 
		this->Insert(0,right); 
}


TMultiDimArray::TMultiDimArray(int _n0, int _n1, double _v) : // 2-d 
TDenseVector(_n0*_n1, _v) 
{
	size_along_dim.resize(2);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1) : // 2-d 
TDenseVector(_n0*_n1)
{
	size_along_dim.resize(2);
        size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, const TDenseVector &right) :
TDenseVector(_n0*_n1),
size_along_dim(2)
{
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
	if (_n0*_n1 < right.dim)
		this->Insert(0,right.SubVector(0,_n0*_n1-1)); 
	else 
		this->Insert(0,right); 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, double _v) :  // 3-d
TDenseVector(_n0*_n1*_n2, _v)
{
	size_along_dim.resize(3);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2) :  // 3-d
TDenseVector(_n0*_n1*_n2)
{
	size_along_dim.resize(3);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, const TDenseVector &right) :  // 3-d
TDenseVector(_n0*_n1*_n2),
size_along_dim(3)
{
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	if (_n0*_n1*_n2 < right.dim)
		this->Insert(0,right.SubVector(0,_n0*_n1*_n2-1)); 
	else 
		this->Insert(0,right); 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, double _v) : // 4-d
TDenseVector(_n0*_n1*_n2*_n3, _v)
{
	size_along_dim.resize(4);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	size_along_dim[3] = _n3; 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3) : // 4-d
TDenseVector(_n0*_n1*_n2*_n3)
{
	size_along_dim.resize(4);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	size_along_dim[3] = _n3; 
}

 
TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, const TDenseVector &right) : // 4-d
TDenseVector(_n0*_n1*_n2*_n3),
size_along_dim(4)
{
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	size_along_dim[3] = _n3; 
	if (_n0*_n1*_n2*_n3 < right.dim)
		this->Insert(0,right.SubVector(0,_n0*_n1*_n2*_n3-1)); 
	else 
		this->Insert(0,right); 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, double _v) :// 5-d
TDenseVector(_n0*_n1*_n2*_n3*_n4, _v) 
{
	size_along_dim.resize(5);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	size_along_dim[3] = _n3; 
	size_along_dim[4] = _n4;
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4) :// 5-d
TDenseVector(_n0*_n1*_n2*_n3*_n4) 
{
	size_along_dim.resize(5);
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	size_along_dim[3] = _n3; 
	size_along_dim[4] = _n4;
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, const TDenseVector &right) :// 5-d
TDenseVector(_n0*_n1*_n2*_n3*_n4), 
size_along_dim(5)
{
	size_along_dim[0] = _n0; 
	size_along_dim[1] = _n1; 
	size_along_dim[2] = _n2; 
	size_along_dim[3] = _n3; 
	size_along_dim[4] = _n4;
	if (_n0*_n1*_n2*_n3*_n4 < right.dim)
		this->Insert(0,right.SubVector(0,_n0*_n1*_n2*_n3*_n4-1)); 
	else 
		this->Insert(0,right); 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, double _v) : // 6-d
TDenseVector(_n0*_n1*_n2*_n3*_n4*_n5, _v) 
{
	size_along_dim.resize(6);
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
        size_along_dim[2] = _n2;
        size_along_dim[3] = _n3;
	size_along_dim[4] = _n4; 
	size_along_dim[5] = _n5;
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5) : // 6-d
TDenseVector(_n0*_n1*_n2*_n3*_n4*_n5)
{
	size_along_dim.resize(6);
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
        size_along_dim[2] = _n2;
        size_along_dim[3] = _n3;
	size_along_dim[4] = _n4; 
	size_along_dim[5] = _n5;
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, const TDenseVector &right) : // 6-d
TDenseVector(_n0*_n1*_n2*_n3*_n4*_n5), 
size_along_dim(6)
{
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
        size_along_dim[2] = _n2;
        size_along_dim[3] = _n3;
	size_along_dim[4] = _n4; 
	size_along_dim[5] = _n5;
	if (_n0*_n1*_n2*_n3*_n4*_n5 < right.dim)
		this->Insert(0,right.SubVector(0,_n0*_n1*_n2*_n3*_n4*_n5-1)); 
	else 
		this->Insert(0,right); 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6, double _v) : // 7-d
TDenseVector(_n0*_n1*_n2*_n3*_n4*_n5*_n6, _v)
{
	size_along_dim.resize(7); 
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
        size_along_dim[2] = _n2;
        size_along_dim[3] = _n3;
        size_along_dim[4] = _n4;
        size_along_dim[5] = _n5;
	size_along_dim[6] = _n6; 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6) : // 7-d
TDenseVector(_n0*_n1*_n2*_n3*_n4*_n5*_n6) 
{
	size_along_dim.resize(7); 
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
        size_along_dim[2] = _n2;
        size_along_dim[3] = _n3;
        size_along_dim[4] = _n4;
        size_along_dim[5] = _n5;
	size_along_dim[6] = _n6; 
}

TMultiDimArray::TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6, const TDenseVector &right) : // 7-d
TDenseVector(_n0*_n1*_n2*_n3*_n4*_n5*_n6), 
size_along_dim(7)
{
	size_along_dim[0] = _n0;
        size_along_dim[1] = _n1;
        size_along_dim[2] = _n2;
        size_along_dim[3] = _n3;
        size_along_dim[4] = _n4;
        size_along_dim[5] = _n5;
	size_along_dim[6] = _n6; 
	if (_n0*_n1*_n2*_n3*_n4*_n5*_n6 < right.dim)
		this->Insert(0,right.SubVector(0,_n0*_n1*_n2*_n3*_n4*_n5*_n6-1)); 
	else 
		this->Insert(0,right); 
}

TMultiDimArray::TMultiDimArray(const TMultiDimArray &right) : 
TDenseVector(right.dim),
size_along_dim(right.size_along_dim)
{
	this->Insert(0,right); 
}

int TMultiDimArray::Size(int d) const
{
	if (d >= Dim())
		throw dw_exception("TMultiDimArray::Size() : index exceeds dimension"); 
	return size_along_dim[d]; 
}

// TMultiDimArray::Reshape
void TMultiDimArray::Reshape(int _n)
{
	size_along_dim.resize(1); 
	size_along_dim[0] = _n; 
	if (this->dim != _n)
	{
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(int _n0, int _n1)
{
	size_along_dim.resize(2); 
	size_along_dim[0] = _n0;
	size_along_dim[1] = _n1;  
	if (this->dim != _n0*_n1)
	{
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n0*_n1); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else 
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(int _n0, int _n1, int _n2)
{
	size_along_dim.resize(3); 
	size_along_dim[0] = _n0;
	size_along_dim[1] = _n1;
	size_along_dim[2] = _n2;   
	if (this->dim != _n0*_n1*_n2)
	{
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n0*_n1*_n2); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(int _n0, int _n1, int _n2, int _n3)
{
	size_along_dim.resize(4); 
	size_along_dim[0] = _n0;
	size_along_dim[1] = _n1;
	size_along_dim[2] = _n2;
	size_along_dim[3] = _n3; 
	if (this->dim != _n0*_n1*_n2*_n3)
	{   
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n0*_n1*_n2*_n3); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else 
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(int _n0, int _n1, int _n2, int _n3, int _n4)
{
	size_along_dim.resize(5); 
	size_along_dim[0] = _n0;
	size_along_dim[1] = _n1;
	size_along_dim[2] = _n2;
	size_along_dim[3] = _n3;
	size_along_dim[4] = _n4;     
	if (this->dim != _n0*_n1*_n2*_n3*_n4)
        {
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n0*_n1*_n2*_n3*_n4); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5)
{
	size_along_dim.resize(6); 
	size_along_dim[0] = _n0;
	size_along_dim[1] = _n1;
	size_along_dim[2] = _n2;
	size_along_dim[3] = _n3;
	size_along_dim[4] = _n4;
	size_along_dim[5] = _n5;      
	if (this->dim != _n0*_n1*_n2*_n3*_n4*_n5)
	{
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n0*_n1*_n2*_n3*_n4*_n5); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else 
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6)
{
	size_along_dim.resize(7); 
	size_along_dim[0] = _n0;
	size_along_dim[1] = _n1;
	size_along_dim[2] = _n2;
	size_along_dim[3] = _n3;
	size_along_dim[4] = _n4;
	size_along_dim[5] = _n5;
	size_along_dim[6] = _n6;       
	if (this->dim != _n0*_n1*_n2*_n3*_n4*_n5*_n6)
        {
		TDenseVector original; 
		original.CopyContent(*this); 
		this->Resize(_n0*_n1*_n2*_n3*_n4*_n5*_n6); 
		if (this->dim >= original.dim)
			this->Insert(0,original); 
		else
			this->Insert(0,original.SubVector(0,this->dim-1)); 
	}
}

void TMultiDimArray::Reshape(const std::vector<int> & _size_spec)
{
	size_along_dim = _size_spec; 
	int total_length = 1; 
	for (int i=0; i<(int)size_along_dim.size(); i++)
		total_length *= size_along_dim[i]; 
	if (this->dim != total_length)
	{
		TDenseVector original;
                original.CopyContent(*this);
                this->Resize(total_length);
                if (this->dim >= original.dim)
                        this->Insert(0,original);
                else
                        this->Insert(0,original.SubVector(0,this->dim-1));
	}
}

// Access with 1 index 
double TMultiDimArray::operator() (int INDEX, bool scalar_flag) const
{
	if(!scalar_flag)
		throw dw_exception("TMultiDimArray::operator() : flag incorrect"); 
	if (INDEX<0 || INDEX>=size_along_dim[0])
		throw dw_exception("TMultiDimArray::operator(): index exceeds size"); 
	else if ( Dim()==1 )
		return TDenseVector::operator()(INDEX); 
	else 
		throw dw_exception("TMultiDimArray::operator() : returning value should not be double but TMultiDimArray"); 
}

void TMultiDimArray::Set(double v, int INDEX)
{
	if (INDEX<0 || INDEX>=size_along_dim[0])
		throw dw_exception("TMultiDimArray::Set() : index exceeds size"); 
	else if ( Dim() == 1)
		TDenseVector::SetElement(v,INDEX); 
	else 
		throw dw_exception("TMultiDimArray::Set() : input should not be double but TMultiDimArray."); 
}

TMultiDimArray TMultiDimArray::operator()(int INDEX) const
{
	if (INDEX<0 || INDEX>=size_along_dim[0])
		throw dw_exception("TMultiDimArray::operator() : index exceeds size");
	int stride = size_along_dim[0]; 
	TMultiDimArray result(this->dim/stride,0.0); 
	result.Insert(0, this->SubVector(TIndex(INDEX,stride,this->dim-1)));
	std::vector<int> result_size_along_dim(this->size_along_dim.begin()+1, this->size_along_dim.end());
	result.Reshape(result_size_along_dim);  
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int INDEX)
{
	if (INDEX<0 || INDEX>=size_along_dim[0])
                throw dw_exception("TMultiDimArray::Set() : index exceeds size");
	int stride = size_along_dim[0]; 
	if(v.dim != this->dim/stride)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	this->Insert(TIndex(INDEX, stride, this->dim-1),v);
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &INDEX) const
{
	for (int i=0; i<INDEX.size; i++)
	{
		if (INDEX[i]<0 || INDEX[i]>=size_along_dim[0])
			throw dw_exception("TMultiDimArray::operator() : index exceeds size"); 
	}
	int stride = size_along_dim[0];
	TMultiDimArray result(INDEX.size * this->dim/stride,0.0);
	for (int i=0; i<INDEX.size; i++)
		result.Insert(TIndex(i,INDEX.size,result.dim-1),this->SubVector(TIndex(INDEX[i],stride,this->dim-1))); 
	std::vector<int> result_size_along_dim(this->size_along_dim.begin()+1, this->size_along_dim.end());
	result_size_along_dim.insert(result_size_along_dim.begin(),INDEX.size);
        result.Reshape(result_size_along_dim);
	return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &INDEX)
{
	for (int i=0; i<INDEX.size; i++)
        {
                if (INDEX[i]<0 || INDEX[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::Set() : index exceeds size");
        }
	int stride = size_along_dim[0]; 
	if (v.dim != this->dim/stride*INDEX.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	for (int i=0; i<INDEX.size; i++)
		this->Insert(TIndex(INDEX[i],stride,this->dim-1),v.SubVector(TIndex(i,INDEX.size, v.dim-1))); 
}

// Access with 2 indices
double TMultiDimArray::operator()(int I, int J, bool scalar_flag) const
{
	if (!scalar_flag)
		throw dw_exception("TMultiDimArray::operator() : flag incorrect");
	if (Dim() != 2)
		throw dw_exception("TMultiDimArray::operator() : incorrect number of indices"); 
	if (I<0 || I>=size_along_dim[0] || J<0 || J>=size_along_dim[1])
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes"); 
	return TDenseVector::operator()(I+J*size_along_dim[0]); 	
}

void TMultiDimArray::Set(double v, int I, int J)
{
	if (Dim() != 2)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	if (I<0 || I>=size_along_dim[0] || J<0 || J>=size_along_dim[1])
                throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
	TDenseVector::SetElement(v,I+J*size_along_dim[0]); 
}

TMultiDimArray TMultiDimArray::operator()(int I, int J) const // (i,j,:,...)
{
	if (Dim() < 2) 
		throw dw_exception("TMultiDimArray::operator() : incorrect number of indices"); 
	if (I<0 || I>=size_along_dim[0] || J<0 || J>=size_along_dim[1])
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	TMultiDimArray result(this->dim/(size_along_dim[0]*size_along_dim[1]),0.0);
	int stride = size_along_dim[0]*size_along_dim[1]; 
	result.Insert(0,this->SubVector(TIndex(I+J*size_along_dim[0],stride,this->dim-1))); 
	std::vector<int> result_size(this->size_along_dim.begin()+2,this->size_along_dim.end()); 
	result.Reshape(result_size);
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, int J) 
{
	if (Dim() < 2)
		throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	if (I<0 || I>=size_along_dim[0] || J<0 || J>=size_along_dim[1])
                throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
	int stride = size_along_dim[0]*size_along_dim[1]; 
	if (v.dim != this->dim/stride )
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match"); 
	this->Insert(TIndex(I+J*size_along_dim[0],stride,this->dim-1), v); 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, const TIndex &J) const // (i1:i2,j1:j2,:,...)
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
	for(int i=0; i<I.size; i++) {
		if (I[i]<0 || I[i]>=size_along_dim[0])
			throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	}
	for (int j=0; j<J.size; j++) {
		if (J[j]<0 || J[j]>=size_along_dim[1])
			throw dw_exception("TMultiDimArray::operator() : indices exeed sizes"); 
	}
	int stride = size_along_dim[0]*size_along_dim[1]; 
	TMultiDimArray result(this->dim/stride*I.size*J.size,0.0); 
	for (int j=0; j<I.size; j++) {
		for (int i=0; i<I.size; i++) 
			result.Insert(TIndex(i+j*I.size,I.size*J.size,result.dim-1),this->SubVector(TIndex(I[i]+J[j]*size_along_dim[0],stride,this->dim-1))); 
	}
	std::vector<int> result_size(this->size_along_dim.begin()+2,this->size_along_dim.end()); 
	result_size.insert(result_size.begin(), J.size); 
	result_size.insert(result_size.begin(), I.size);
	result.Reshape(result_size); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J) 
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	int stride = size_along_dim[0]*size_along_dim[1];
	if (v.dim != this->dim/stride*I.size*J.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
        for(int i=0; i<I.size; i++) {
                if (I[i]<0 || I[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
        }
        for (int j=0; j<J.size; j++) {
                if (J[j]<0 || J[j]>=size_along_dim[1])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");	
	}
	for (int j=0; j<J.size; j++) {
		for (int i=0; i<I.size; i++)
			this->Insert(TIndex(I[i]+J[j]*size_along_dim[0],stride,this->dim-1),v.SubVector(TIndex(i+j*I.size,I.size*J.size,v.dim-1))); 
	}
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, int J) const
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
	if (J<0 || J>=size_along_dim[1])
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	for(int i=0; i<I.size; i++) {
                if (I[i]<0 || I[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
        }
        int stride = size_along_dim[0]*size_along_dim[1];
	TMultiDimArray result(this->dim/stride*I.size,0.0); 
	for (int i=0; i<I.size; i++)
		result.Insert(TIndex(i,I.size,result.dim-1),this->SubVector(TIndex(I[i]+J*size_along_dim[0],stride,this->dim-1))); 
	std::vector<int> result_size(this->size_along_dim.begin()+2,this->size_along_dim.end()); 
	result_size.insert(result_size.begin(),I.size); 
	result.Reshape(result_size); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, int J) 
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	if (J<0 || J>=size_along_dim[1])
                throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
        int stride = size_along_dim[0]*size_along_dim[1];
        if (v.dim != this->dim/stride*I.size)
                throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
        for(int i=0; i<I.size; i++) {
                if (I[i]<0 || I[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
        }
	for (int i=0; i<I.size; i++) 
		this->Insert(TIndex(I[i]+J*size_along_dim[0],stride,this->dim-1),v.SubVector(TIndex(i,I.size,v.dim-1)) ); 

}

TMultiDimArray TMultiDimArray::operator()(int I, const TIndex &J) const
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
	if (I<0 || I>=size_along_dim[0])
		throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        for (int j=0; j<J.size; j++) {
                if (J[j]<0 || J[j]>=size_along_dim[1])
                        throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        }
        int stride = size_along_dim[0]*size_along_dim[1];
        TMultiDimArray result(this->dim/stride*J.size,0.0);
        for (int j=0; j<J.size; j++)
        	result.Insert(TIndex(j,J.size,result.dim-1),this->SubVector(TIndex(I+J[j]*size_along_dim[0],stride,this->dim-1)));
        
        std::vector<int> result_size(size_along_dim.begin()+2,size_along_dim.end());
        result_size.insert(result_size.begin(), J.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, const TIndex &J)
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
        if (I<0 || I>=size_along_dim[0])
                throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
	int stride = size_along_dim[0]*size_along_dim[1];
	if(v.dim != this->dim/stride*J.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	for (int j=0; j<J.size; j++)
		this->Insert(TIndex(I+J[j]*size_along_dim[0],stride,this->dim-1),v.SubVector(TIndex(j,J.size,v.dim-1))); 
}

// Access with 3 indices
TMultiDimArray TMultiDimArray::operator()(const TIndex &I, const TIndex &J, const TIndex &K) const 
{
	if (Dim() < 3)
		throw dw_exception("TMultiDimArray::operator() : incorrect number of indices"); 
	for (int i=0; i<I.size; i++) {
		if (I[i]<0 || I[i]>=size_along_dim[0])
			throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
	}
	for (int j=0; j<J.size; j++) {
                if (J[j]<0 || J[j]>=size_along_dim[1])
                        throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        }
	for (int k=0; k<K.size; k++) {
		if (K[k]<0 || K[k]>=size_along_dim[2])
			throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
	}
	int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2]; 
	TMultiDimArray result(this->dim/stride*I.size*J.size*K.size,0.0); 
	for (int k=0; k<K.size; k++)
		for (int j=0; j<J.size; j++)
			for (int i=0; i<I.size; i++)
				result.Insert(TIndex(i+j*I.size+k*I.size*J.size, I.size*J.size*K.size,result.dim-1),this->SubVector(TIndex(I[i]+J[j]*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1)));
	std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end()); 
	result_size.insert(result_size.begin(),K.size); 
	result_size.insert(result_size.begin(),J.size); 
	result_size.insert(result_size.begin(),I.size); 
	result.Reshape(result_size); 
	return result;  
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J, const TIndex &K)
{
	if (Dim() < 3)
		throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2]; 
	if(v.dim != this->dim/stride*I.size*J.size*K.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	for (int i=0; i<I.size; i++) {
                if (I[i]<0 || I[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
        for (int j=0; j<J.size; j++) {
                if (J[j]<0 || J[j]>=size_along_dim[1])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
        for (int k=0; k<K.size; k++) {
                if (K[k]<0 || K[k]>=size_along_dim[2])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
	for (int k=0; k<K.size; k++)
		for (int j=0; j<J.size; j++)
			for (int i=0; i<I.size; i++)
				this->Insert(TIndex(I[i]+J[j]*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1), v.SubVector(TIndex(i+j*I.size+k*I.size*J.size, I.size*J.size*K.size, v.dim-1))); 
}

TMultiDimArray TMultiDimArray::operator()(int I, const TIndex &J, const TIndex &K) const
{
	if (Dim() < 3)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
        if (I<0 || I>=size_along_dim[0])
                throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        for (int j=0; j<J.size; j++) {
                if (J[j]<0 || J[j]>=size_along_dim[1])
                        throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        }
        for (int k=0; k<K.size; k++) {
                if (K[k]<0 || K[k]>=size_along_dim[2])
                        throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        }
	int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2]; 
	TMultiDimArray result(this->dim/stride*J.size*K.size, 0.0); 
	for (int k=0; k<K.size; k++)
		for (int j=0; j<J.size; j++)
			result.Insert(TIndex(j+k*J.size,J.size*K.size,result.dim-1), this->SubVector(TIndex(I+J[j]*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1))); 
	std::vector<int>result_size(size_along_dim.begin()+3,size_along_dim.end()); 
	result_size.insert(result_size.begin(),K.size); 
	result_size.insert(result_size.begin(),J.size); 
	result.Reshape(result_size); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, const TIndex &J, const TIndex &K)
{
	if (Dim() < 3)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
        int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2];
        if(v.dim != this->dim/stride*J.size*K.size)
                throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
        if (I<0 || I>=size_along_dim[0])
                throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        for (int j=0; j<J.size; j++) {
                if (J[j]<0 || J[j]>=size_along_dim[1])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
        for (int k=0; k<K.size; k++) {
                if (K[k]<0 || K[k]>=size_along_dim[2])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
        for (int k=0; k<K.size; k++)
                for (int j=0; j<J.size; j++)
                        this->Insert(TIndex(I+J[j]*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1), v.SubVector(TIndex(j+k*J.size, J.size*K.size, v.dim-1)));
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, int J, const TIndex &K) const
{
        if (Dim() < 3)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
        for (int i=0; i<I.size; i++) {
                if (I[i]<0 || I[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        }
        if (J<0 || J>=size_along_dim[1])
                throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        for (int k=0; k<K.size; k++) {
                if (K[k]<0 || K[k]>=size_along_dim[2])
                        throw dw_exception("TMultiDimArray::operator() : indices exeed sizes");
        }
        int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2];
        TMultiDimArray result(this->dim/stride*I.size*K.size,0.0);
        for (int k=0; k<K.size; k++)
		for (int i=0; i<I.size; i++)
               		result.Insert(TIndex(i+k*I.size, I.size*K.size, result.dim-1),this->SubVector(TIndex(I[i]+J*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1)));
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result_size.insert(result_size.begin(),K.size);
        result_size.insert(result_size.begin(),I.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, int J, const TIndex &K)
{
	if (Dim() < 3)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
        int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2];
        if(v.dim != this->dim/stride*I.size*K.size)
                throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
        for (int i=0; i<I.size; i++) {
                if (I[i]<0 || I[i]>=size_along_dim[0])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
        if (J<0 || J>=size_along_dim[1])
        	throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        for (int k=0; k<K.size; k++) {
                if (K[k]<0 || K[k]>=size_along_dim[2])
                        throw dw_exception("TMultiDimArray::Set() : indices exeed sizes");
        }
        for (int k=0; k<K.size; k++)
        	for (int i=0; i<I.size; i++)
                	this->Insert(TIndex(I[i]+J*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1), v.SubVector(TIndex(i+k*I.size, I.size*K.size, v.dim-1)));
}
