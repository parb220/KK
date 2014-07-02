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

// Check if index is within the legal range
bool TMultiDimArray::out_of_range(const TIndex &INDEX, int size_limit) const
{
	for (int i=0; i<INDEX.size; i++)
		if (INDEX[i]<0 || INDEX[i] >= size_limit)
			return true;  
	return false; 
}

// Access with 1 index 
double TMultiDimArray::operator() (int INDEX, bool scalar_flag) const
{
	if(!scalar_flag)
		throw dw_exception("TMultiDimArray::operator() : flag incorrect"); 
	if(out_of_range(TIndex(INDEX), size_along_dim[0]) )
		throw dw_exception("TMultiDimArray::operator(): index exceeds size"); 
	else if ( Dim()==1 )
		return TDenseVector::operator()(INDEX); 
	else 
		throw dw_exception("TMultiDimArray::operator() : returning value should not be double but TMultiDimArray"); 
}

void TMultiDimArray::Set(double v, int INDEX)
{
	if (out_of_range(TIndex(INDEX), size_along_dim[0]))
		throw dw_exception("TMultiDimArray::Set() : index exceeds size"); 
	else if ( Dim() == 1)
		TDenseVector::SetElement(v,INDEX); 
	else 
		throw dw_exception("TMultiDimArray::Set() : input should not be double but TMultiDimArray."); 
}

TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &INDEX) const
{
	if (Dim() < 1)
		throw dw_exception("TMultiDimArray::operator() : incorrect number of index");
	if (out_of_range(INDEX, size_along_dim[0]))
		throw dw_exception("TMultiDimArray::operator() : index exceeds size"); 
	int stride = size_along_dim[0];
	TMultiDimArray result(INDEX.size * this->dim/stride,0.0);
	for (int i=0; i<INDEX.size; i++)
		result.Insert(TIndex(i,INDEX.size,result.dim-1),this->SubVector(TIndex(INDEX[i],stride,this->dim-1))); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &INDEX)
{
	if (out_of_range(INDEX, size_along_dim[0]))
        	throw dw_exception("TMultiDimArray::Set() : index exceeds size");
	int stride = size_along_dim[0]; 
	if (v.dim != this->dim/stride*INDEX.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	for (int i=0; i<INDEX.size; i++)
		this->Insert(TIndex(INDEX[i],stride,this->dim-1),v.SubVector(TIndex(i,INDEX.size, v.dim-1))); 
}

TMultiDimArray TMultiDimArray::operator()(int INDEX) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(INDEX))); 
	std::vector<int> result_size_along_dim(this->size_along_dim.begin()+1, this->size_along_dim.end());
	result.Reshape(result_size_along_dim);  
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int INDEX)
{
	Set(v,TIndex(INDEX));
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &INDEX) const
{
	TMultiDimArray result(ExtractSubMatrix(INDEX));  
	std::vector<int> result_size_along_dim(this->size_along_dim.begin()+1, this->size_along_dim.end());
        result_size_along_dim.insert(result_size_along_dim.begin(),INDEX.size);
	result.Reshape(result_size_along_dim); 
	return result; 
}

// Access with 2 indices
double TMultiDimArray::operator()(int I, int J, bool scalar_flag) const
{
	if (!scalar_flag)
		throw dw_exception("TMultiDimArray::operator() : flag incorrect");
	if (Dim() != 2)
		throw dw_exception("TMultiDimArray::operator() : incorrect number of indices"); 
	if (out_of_range(TIndex(I),size_along_dim[0]) || out_of_range(TIndex(J),size_along_dim[1]))
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes"); 
	return TDenseVector::operator()(I+J*size_along_dim[0]); 	
}

void TMultiDimArray::Set(double v, int I, int J)
{
	if (Dim() != 2)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	if (out_of_range(TIndex(I),size_along_dim[0]), out_of_range(TIndex(J),size_along_dim[1]))
                throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
	TDenseVector::SetElement(v,I+J*size_along_dim[0]); 
}

TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &I, const TIndex &J) const // (i1:i2,j1:j2,:,...)
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
	if (out_of_range(I,size_along_dim[0]) || out_of_range(J,size_along_dim[1]))
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes"); 
	int stride = size_along_dim[0]*size_along_dim[1]; 
	TMultiDimArray result(this->dim/stride*I.size*J.size,0.0); 
	for (int j=0; j<I.size; j++) {
		for (int i=0; i<I.size; i++) 
			result.Insert(TIndex(i+j*I.size,I.size*J.size,result.dim-1),this->SubVector(TIndex(I[i]+J[j]*size_along_dim[0],stride,this->dim-1))); 
	}
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J) 
{
	if (Dim() < 2)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	int stride = size_along_dim[0]*size_along_dim[1];
	if (v.dim != this->dim/stride*I.size*J.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	if (out_of_range(I,size_along_dim[0]) || out_of_range(J,size_along_dim[1]))
        	throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
	for (int j=0; j<J.size; j++) {
		for (int i=0; i<I.size; i++)
			this->Insert(TIndex(I[i]+J[j]*size_along_dim[0],stride,this->dim-1),v.SubVector(TIndex(i+j*I.size,I.size*J.size,v.dim-1))); 
	}
}

TMultiDimArray TMultiDimArray::operator()(int I, int J) const // (i,j,:,...)
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(I),TIndex(J)));
	std::vector<int> result_size(this->size_along_dim.begin()+2,this->size_along_dim.end()); 
	result.Reshape(result_size);
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, int J) 
{
	Set(v,TIndex(I),TIndex(J)); 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, const TIndex &J) const
{
	TMultiDimArray result(ExtractSubMatrix(I,J)); 
	std::vector<int> result_size(this->size_along_dim.begin()+2,this->size_along_dim.end()); 
	result_size.insert(result_size.begin(), J.size); 
	result_size.insert(result_size.begin(), I.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, int J) const
{
	TMultiDimArray result(ExtractSubMatrix(I,TIndex(J))); 
	std::vector<int> result_size(this->size_along_dim.begin()+2,this->size_along_dim.end()); 
	result_size.insert(result_size.begin(),I.size); 
	result.Reshape(result_size); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, int J) 
{
	Set(v,I,TIndex(J)); 
}

TMultiDimArray TMultiDimArray::operator()(int I, const TIndex &J) const
{
        TMultiDimArray result(ExtractSubMatrix(TIndex(I),J)); 
        std::vector<int> result_size(size_along_dim.begin()+2,size_along_dim.end());
        result_size.insert(result_size.begin(), J.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, const TIndex &J)
{
	Set(v,TIndex(I),J); 
}

// Access with 3 indices
TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &I, const TIndex &J, const TIndex &K) const 
{
	if (Dim() < 3)
		throw dw_exception("TMultiDimArray::operator() : incorrect number of indices"); 
	if (out_of_range(I,size_along_dim[0]) || out_of_range(J,size_along_dim[1]) || out_of_range(K,size_along_dim[2]))
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2]; 
	TMultiDimArray result(this->dim/stride*I.size*J.size*K.size,0.0); 
	for (int k=0; k<K.size; k++)
		for (int j=0; j<J.size; j++)
			for (int i=0; i<I.size; i++)
				result.Insert(TIndex(i+j*I.size+k*I.size*J.size, I.size*J.size*K.size,result.dim-1),this->SubVector(TIndex(I[i]+J[j]*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1)));
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J, const TIndex &K)
{
	if (Dim() < 3)
		throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	int stride = size_along_dim[0] * size_along_dim[1] * size_along_dim[2]; 
	if(v.dim != this->dim/stride*I.size*J.size*K.size)
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
	if (out_of_range(I,size_along_dim[0]) || out_of_range(J,size_along_dim[1]) || out_of_range(K,size_along_dim[2]))
                throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
	for (int k=0; k<K.size; k++)
		for (int j=0; j<J.size; j++)
			for (int i=0; i<I.size; i++)
				this->Insert(TIndex(I[i]+J[j]*size_along_dim[0]+K[k]*size_along_dim[0]*size_along_dim[1], stride, this->dim-1), v.SubVector(TIndex(i+j*I.size+k*I.size*J.size, I.size*J.size*K.size, v.dim-1))); 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, const TIndex &J, const TIndex &K) const
{
	TMultiDimArray result(ExtractSubMatrix(I,J,K)); 
	std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end()); 
	result_size.insert(result_size.begin(),K.size); 
	result_size.insert(result_size.begin(),J.size); 
	result_size.insert(result_size.begin(),I.size); 
	result.Reshape(result_size); 
	return result;  
}

TMultiDimArray TMultiDimArray::operator()(int I, const TIndex &J, const TIndex &K) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(I),J,K)); 
	std::vector<int>result_size(size_along_dim.begin()+3,size_along_dim.end()); 
	result_size.insert(result_size.begin(),K.size); 
	result_size.insert(result_size.begin(),J.size); 
	result.Reshape(result_size); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, const TIndex &J, const TIndex &K)
{
	Set(v,TIndex(I),J,K); 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, int J, const TIndex &K) const
{
        TMultiDimArray result(ExtractSubMatrix(I,TIndex(J),K)); 
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result_size.insert(result_size.begin(),K.size);
        result_size.insert(result_size.begin(),I.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, int J, const TIndex &K)
{
	Set(v,I,TIndex(J),K); 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, const TIndex &J, int K) const
{
        TMultiDimArray result(ExtractSubMatrix(I,J,TIndex(K))); 
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result_size.insert(result_size.begin(),J.size);
        result_size.insert(result_size.begin(),I.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J, int K)
{
	Set(v,I,J,TIndex(K)); 
}

TMultiDimArray TMultiDimArray::operator()(int I, int J, const TIndex &K) const
{
        TMultiDimArray result(ExtractSubMatrix(TIndex(I),TIndex(J),K));
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result_size.insert(result_size.begin(),K.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, int J, const TIndex &K) 
{
	Set(v,TIndex(I),TIndex(J),K); 
}

TMultiDimArray TMultiDimArray::operator()(int I, const TIndex & J, int K) const
{
        TMultiDimArray result(ExtractSubMatrix(TIndex(I),J,TIndex(K))); 
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result_size.insert(result_size.begin(),J.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, const TIndex &J, int K)
{
	Set(v,TIndex(I),J,TIndex(K));
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &I, int J, int K) const
{
        TMultiDimArray result(ExtractSubMatrix(I,TIndex(J),TIndex(K))); 
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result_size.insert(result_size.begin(),I.size);
        result.Reshape(result_size);
        return result;
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &I, int J, int K)
{
	Set(v,I,TIndex(J),TIndex(K)); 
}

TMultiDimArray TMultiDimArray:: operator()(int I, int J, int K) const
{
        TMultiDimArray result(ExtractSubMatrix(TIndex(I),TIndex(J),TIndex(K))); 
        std::vector<int> result_size(size_along_dim.begin()+3,size_along_dim.end());
        result.Reshape(result_size);
        return result;

}

void TMultiDimArray::Set(const TMultiDimArray &v, int I, int J, int K)
{
	Set(v,TIndex(I),TIndex(J),TIndex(K)); 
}

double TMultiDimArray::operator()(int I, int J, int K, bool scalar_flag) const
{
	if (!scalar_flag)
                throw dw_exception("TMultiDimArray::operator() : flag incorrect");
        if (Dim() != 3)
                throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
	if (out_of_range(TIndex(I),size_along_dim[0]) || out_of_range(TIndex(J),size_along_dim[1]) || out_of_range(TIndex(K),size_along_dim[2]))
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
        return TDenseVector::operator()(I+J*size_along_dim[0]+K*size_along_dim[0]*size_along_dim[1]);
}

void TMultiDimArray::Set(double v, int I, int J, int K) 
{
	if (Dim() != 3)
		throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	if (out_of_range(TIndex(I),size_along_dim[0]) || out_of_range(TIndex(J),size_along_dim[1]) || out_of_range(TIndex(K),size_along_dim[2]))
                throw dw_exception("TMultiDimArray::Set() : indices exceed sizes");
	TDenseVector::SetElement(v, I+J*size_along_dim[0]+K*size_along_dim[0]*size_along_dim[1]); 
}

// Access with 4 indices
TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3) const
{
	if (Dim() < 4)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3])) 
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	int stride = size_along_dim[3] * size_along_dim[2] * size_along_dim[1] * size_along_dim[0]; 
	TMultiDimArray result(this->dim/stride*n0.size*n1.size*n2.size*n3.size,0.0); 
	for (int i3=0; i3<n3.size; i3++)
		for (int i2=0; i2<n2.size; i2++)
			for (int i1=0; i1<n1.size; i1++)
				for (int i0=0; i0<n0.size; i0++)
					result.Insert(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size, n0.size*n1.size*n2.size*n3.size, result.dim-1), this->SubVector(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2], stride, this->dim-1)));

	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,n2)); 
	std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size); 
	result_size.insert(result_size.begin(),n2.size); 
	result_size.insert(result_size.begin(),n1.size); 
	result_size.insert(result_size.begin(),n0.size); 
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3) const
{
        TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,TIndex(n3))); 
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n2.size);
        result_size.insert(result_size.begin(),n1.size);
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3) const
{
        TMultiDimArray result(ExtractSubMatrix(n0,n1,TIndex(n2),n3)); 

        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n3.size);
        result_size.insert(result_size.begin(),n1.size);
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),n2,n3)); 
	std::vector<int> result_size(size_along_dim.begin()+4,size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size); 
	result_size.insert(result_size.begin(),n2.size); 
	result_size.insert(result_size.begin(),n0.size); 
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,n2,n3)); 
	std::vector<int> result_size(size_along_dim.begin()+4,size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size); 
	result_size.insert(result_size.begin(),n2.size); 
	result_size.insert(result_size.begin(),n1.size); 
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, int n2, int n3) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,TIndex(n2),TIndex(n3))); 
	std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end()); 
	result_size.insert(result_size.begin(),n1.size); 
	result_size.insert(result_size.begin(),n0.size); 
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, const TIndex &n2, int n3) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),n2,TIndex(n3)));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n2.size);
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, int n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),TIndex(n2),n3));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n3.size);
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, const TIndex &n2, int n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,n2,TIndex(n3)));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n2.size);
        result_size.insert(result_size.begin(),n1.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, int n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,TIndex(n2),n3));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n3.size);
        result_size.insert(result_size.begin(),n1.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, const TIndex &n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),n2,n3));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n3.size);
        result_size.insert(result_size.begin(),n2.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, int n2, const TIndex &n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),TIndex(n2),n3)); 
	std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size); 
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, const TIndex & n2, int n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),n2,TIndex(n3)));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n2.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, int n2, int n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,TIndex(n2),TIndex(n3)));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n1.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, int n2, int n3) const 
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),TIndex(n2),TIndex(n3)));
        std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, int n2, int n3) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3))); 
	std::vector<int>result_size(size_along_dim.begin()+4,size_along_dim.end());
	result.Reshape(result_size);
        return result;
}

double TMultiDimArray::operator()(int n0, int n1, int n2, int n3, bool scalar_flag) const
{
	if(!scalar_flag)
                throw dw_exception("TMultiDimArray::operator() : flag incorrect");
         if (out_of_range(TIndex(n0),size_along_dim[0]) || out_of_range(TIndex(n1),size_along_dim[1]) || out_of_range(TIndex(n2),size_along_dim[2]) || out_of_range(TIndex(n3),size_along_dim[3])) 
                throw dw_exception("TMultiDimArray::operator(): indices exceed sizes");
        else if ( Dim()==4 )
                return TDenseVector::operator()(n0+n1*size_along_dim[0]+n2*size_along_dim[0]*size_along_dim[1]+n3*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]);
        else
                throw dw_exception("TMultiDimArray::operator() : returning value should not be double but TMultiDimArray");
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3)
{
	if (Dim() < 4)
		throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
	int stride = size_along_dim[3] * size_along_dim[2] * size_along_dim[1] * size_along_dim[0];
	if (v.dim != this->dim/stride*n0.size*n1.size*n2.size*n3.size) 
		throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");	
	 if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3])) 
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	for (int i3=0; i3<n3.size; i3++)
                for (int i2=0; i2<n2.size; i2++)
                        for (int i1=0; i1<n1.size; i1++)
                                for (int i0=0; i0<n0.size; i0++)
					this->Insert(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2], stride, this->dim-1), v.SubVector(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size, n0.size*n1.size*n2.size*n3.size, v.dim-1))); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3)
{
	Set(v,n0,n1,n2,TIndex(n3)); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3)
{
	Set(v,n0,n1,TIndex(n2),n3); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3)
{
	Set(v,n0,TIndex(n1),n2,n3); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3)
{
	Set(v,TIndex(n0),n1,n2,n3); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3)
{
	Set(v,n0,n1,TIndex(n2),TIndex(n3)); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3)
{
	Set(v,n0,TIndex(n1),n2,TIndex(n3));
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3)
{
	Set(v,n0,TIndex(n1),TIndex(n2),n3); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3)
{
	Set(v,TIndex(n0),n1,n2,TIndex(n3)); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3)
{
	Set(v,TIndex(n0),n1,TIndex(n2),n3); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3)
{
	Set(v,TIndex(n0),TIndex(n1),n2,n3);
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3)
{
	Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3)
{
	Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3)); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3)
{
	Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3)); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3)
{
	Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3)); 
}

void TMultiDimArray::Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3)
{
	Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3)); 
}

void TMultiDimArray::Set(double v, int n0, int n1, int n2, int n3)
{
	 if (out_of_range(TIndex(n0),size_along_dim[0]) || out_of_range(TIndex(n1),size_along_dim[1]) || out_of_range(TIndex(n2),size_along_dim[2]) || out_of_range(TIndex(n3),size_along_dim[3])) 
                throw dw_exception("TMultiDimArray::Set() : index exceeds size");
        else if ( Dim() == 4)
                TDenseVector::SetElement(v,n0+n1*size_along_dim[0]+n2*size_along_dim[0]*size_along_dim[1]+n3*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]);
        else
                throw dw_exception("TMultiDimArray::Set() : input should not be double but TMultiDimArray.");

}

// Access with 5 indices
TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	if (Dim() < 5)
		 throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");		
	if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3]) || out_of_range(n4,size_along_dim[4]) )
		throw dw_exception("TMultiDimArray::operator() : indices exceed sizes"); 

	int stride = size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4];
        TMultiDimArray result(this->dim/stride*n0.size*n1.size*n2.size*n3.size*n4.size,0.0);
	for (int i4=0; i4<n4.size; i4++)
		for (int i3=0; i3<n3.size; i3++)
			for (int i2=0; i2<n2.size; i2++)
				for (int i1=0; i1<n1.size; i1++)
					for (int i0=0; i0<n0.size; i0++)
						result.Insert(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size+i4*n0.size*n1.size*n2.size*n3.size,n0.size*n1.size*n2.size*n3.size*n4.size,result.dim-1),this->SubVector(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]+n4[i4]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3], stride, this->dim-1))); 

        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, int n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),TIndex(n2),n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, const TIndex &n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),n2,TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n2.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),n2,n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, int n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),TIndex(n2),TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),TIndex(n2),n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,TIndex(n2),TIndex(n3),TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, int n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,TIndex(n2),TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,TIndex(n2),n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,n2,TIndex(n3),TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),n2,TIndex(n3),TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,TIndex(n3),TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,TIndex(n2),n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,TIndex(n2),TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),n2,n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),n2,TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),TIndex(n2),n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,n2,n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,n2,TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,TIndex(n2),n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),TIndex(n1),n2,n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,n3,TIndex(n4))); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,TIndex(n3),n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,TIndex(n2),n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n1.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size); 
	return result; 
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,TIndex(n1),n2,n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n0.size);
	result.Reshape(result_size);
	return result; 
}
 
TMultiDimArray TMultiDimArray::operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const
{
	TMultiDimArray result(ExtractSubMatrix(TIndex(n0),n1,n2,n3,n4)); 
	std::vector<int>result_size(size_along_dim.begin()+5, size_along_dim.end()); 
	result_size.insert(result_size.begin(),n4.size); 
	result_size.insert(result_size.begin(),n3.size);
	result_size.insert(result_size.begin(),n2.size);
	result_size.insert(result_size.begin(),n1.size);
	result.Reshape(result_size); 
	return result; 
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4)
{
	if (Dim() < 5)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
        int stride = size_along_dim[4] * size_along_dim[3] * size_along_dim[2] * size_along_dim[1] * size_along_dim[0];
        if (v.dim != this->dim/stride*n0.size*n1.size*n2.size*n3.size*n4.size)
                throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
         if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3]) || out_of_range(n4, size_along_dim[4]))
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	for (int i4=0; i4<n4.size; i4++)
        	for (int i3=0; i3<n3.size; i3++)
                	for (int i2=0; i2<n2.size; i2++)
                        	for (int i1=0; i1<n1.size; i1++)
                                	for (int i0=0; i0<n0.size; i0++)
	                                        this->Insert(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]+n4[i4]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3], stride, this->dim-1), v.SubVector(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size+i4*n0.size*n1.size*n2.size*n3.size, n0.size*n1.size*n2.size*n3.size*n4.size, v.dim-1)));
}

// Access with 6 indices
TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const
{
	if (Dim() < 6)
                 throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
        if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3]) || out_of_range(n4,size_along_dim[4]) || out_of_range(n5,size_along_dim[5]) )
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");

        int stride = size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4]*size_along_dim[5];
        TMultiDimArray result(this->dim/stride*n0.size*n1.size*n2.size*n3.size*n4.size*n5.size,0.0);
	for (int i5=0; i5<n5.size; i5++)
        	for (int i4=0; i4<n4.size; i4++)
                	for (int i3=0; i3<n3.size; i3++)
                        	for (int i2=0; i2<n2.size; i2++)
                                	for (int i1=0; i1<n1.size; i1++)
                                        	for (int i0=0; i0<n0.size; i0++)
                                                	result.Insert(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size+i4*n0.size*n1.size*n2.size*n3.size+i5*n0.size*n1.size*n2.size*n3.size*n4.size,n0.size*n1.size*n2.size*n3.size*n4.size*n5.size,result.dim-1),this->SubVector(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]+n4[i4]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]+n5[i5]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4], stride, this->dim-1)));

        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,n3,n4,n5)); 
	std::vector<int>result_size(size_along_dim.begin()+6, size_along_dim.end());
	result_size.insert(result_size.begin(),n5.size); 
        result_size.insert(result_size.begin(),n4.size);
        result_size.insert(result_size.begin(),n3.size);
        result_size.insert(result_size.begin(),n2.size);
        result_size.insert(result_size.begin(),n1.size);
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;	
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5)
{
	if (Dim() < 6)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
        int stride = size_along_dim[5] * size_along_dim[4] * size_along_dim[3] * size_along_dim[2] * size_along_dim[1] * size_along_dim[0];
        if (v.dim != this->dim/stride*n0.size*n1.size*n2.size*n3.size*n4.size*n5.size)
                throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
         if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3]) || out_of_range(n4, size_along_dim[4]) || out_of_range(n5,size_along_dim[5]) )
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	for (int i5=0; i5<n5.size; i5++)
        	for (int i4=0; i4<n4.size; i4++)
                	for (int i3=0; i3<n3.size; i3++)
                        	for (int i2=0; i2<n2.size; i2++)
                                	for (int i1=0; i1<n1.size; i1++)
                                        	for (int i0=0; i0<n0.size; i0++)
                                                	this->Insert(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]+n4[i4]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]+n5[i5]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4], stride, this->dim-1), v.SubVector(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size+i4*n0.size*n1.size*n2.size*n3.size+i5*n0.size*n1.size*n2.size*n3.size*n4.size, n0.size*n1.size*n2.size*n3.size*n4.size*n5.size, v.dim-1)));
}

// Access with 7 indices
TMultiDimArray TMultiDimArray::ExtractSubMatrix(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const
{
	if (Dim() < 7)
                 throw dw_exception("TMultiDimArray::operator() : incorrect number of indices");
        if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3]) || out_of_range(n4,size_along_dim[4]) || out_of_range(n5,size_along_dim[5]) || out_of_range(n6,size_along_dim[6]) )
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");

        int stride = size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4]*size_along_dim[5]*size_along_dim[6];
        TMultiDimArray result(this->dim/stride*n0.size*n1.size*n2.size*n3.size*n4.size*n5.size*n6.size,0.0);
	for (int i6=0; i6<n6.size; i6++)
		for (int i5=0; i5<n5.size; i5++)
        		for (int i4=0; i4<n4.size; i4++)
                		for (int i3=0; i3<n3.size; i3++)
                        		for (int i2=0; i2<n2.size; i2++)
                                		for (int i1=0; i1<n1.size; i1++)
                                        		for (int i0=0; i0<n0.size; i0++)
                                                		result.Insert(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size+i4*n0.size*n1.size*n2.size*n3.size+i5*n0.size*n1.size*n2.size*n3.size*n4.size+i6*n0.size*n1.size*n2.size*n3.size*n4.size*n5.size, n0.size*n1.size*n2.size*n3.size*n4.size*n5.size*n6.size, result.dim-1),this->SubVector(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]+n4[i4]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]+n5[i5]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4]+n6[i6]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4]*size_along_dim[5], stride, this->dim-1)));

        return result;
}

TMultiDimArray TMultiDimArray::operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const
{
	TMultiDimArray result(ExtractSubMatrix(n0,n1,n2,n3,n4,n5,n6)); 
	std::vector<int>result_size(size_along_dim.begin()+7, size_along_dim.end());
	result_size.insert(result_size.begin(),n6.size); 
	result_size.insert(result_size.begin(),n5.size); 
        result_size.insert(result_size.begin(),n4.size);
        result_size.insert(result_size.begin(),n3.size);
        result_size.insert(result_size.begin(),n2.size);
        result_size.insert(result_size.begin(),n1.size);
        result_size.insert(result_size.begin(),n0.size);
        result.Reshape(result_size);
        return result;	
}

void TMultiDimArray::Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6)
{
	if (Dim() < 7)
                throw dw_exception("TMultiDimArray::Set() : incorrect number of indices");
        int stride = size_along_dim[6] * size_along_dim[5] * size_along_dim[4] * size_along_dim[3] * size_along_dim[2] * size_along_dim[1] * size_along_dim[0];
        if (v.dim != this->dim/stride*n0.size*n1.size*n2.size*n3.size*n4.size*n5.size*n6.size)
                throw dw_exception("TMultiDimArray::Set() : value dimension and sub-matrix dimenstion do not match");
         if (out_of_range(n0,size_along_dim[0]) || out_of_range(n1,size_along_dim[1]) || out_of_range(n2,size_along_dim[2]) || out_of_range(n3,size_along_dim[3]) || out_of_range(n4, size_along_dim[4]) || out_of_range(n5,size_along_dim[5]) || out_of_range(n6,size_along_dim[6]) )
                throw dw_exception("TMultiDimArray::operator() : indices exceed sizes");
	for (int i6=0; i6<n6.size; i6++)
		for (int i5=0; i5<n5.size; i5++)
        		for (int i4=0; i4<n4.size; i4++)
                		for (int i3=0; i3<n3.size; i3++)
                        		for (int i2=0; i2<n2.size; i2++)
                                		for (int i1=0; i1<n1.size; i1++)
                                        		for (int i0=0; i0<n0.size; i0++)
                                                		this->Insert(TIndex(n0[i0]+n1[i1]*size_along_dim[0]+n2[i2]*size_along_dim[0]*size_along_dim[1]+n3[i3]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]+n4[i4]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]+n5[i5]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4]+n6[i6]*size_along_dim[0]*size_along_dim[1]*size_along_dim[2]*size_along_dim[3]*size_along_dim[4]*size_along_dim[5], stride, this->dim-1), v.SubVector(TIndex(i0+i1*n0.size+i2*n0.size*n1.size+i3*n0.size*n1.size*n2.size+i4*n0.size*n1.size*n2.size*n3.size+i5*n0.size*n1.size*n2.size*n3.size*n4.size+i6*n0.size*n1.size*n2.size*n3.size*n4.size*n5.size, n0.size*n1.size*n2.size*n3.size*n4.size*n5.size*n6.size, v.dim-1)));
}


