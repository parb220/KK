#ifndef _MULTI_DIMENSIONAL_ARRAY_HEADER_
#define _MULTI_DIMENSIONAL_ARRAY_HEADER_

#include <vector>
class TDenseVector; 
class TIndex; 
const bool SCALAR = true; 

// TMultiDimArray are column major arranged. 
// Given a TMultiDimArray of n0 by n1 by n2, then the corresponding TDenseVector stores
// 1st n0 elements, 2nd n0 element, ...., n1*n2-th element

class TMultiDimArray : public TDenseVector 
{
private: 
	std::vector<int> size_along_dim; 
	bool out_of_range(const TIndex &, int) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &) const ; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &) const ; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;

public: 
	// Construction
	TMultiDimArray(int _n, double _v); // 1-d
	TMultiDimArray(int _n); 
	TMultiDimArray(int _n, const TDenseVector &); 
	TMultiDimArray(int _n0, int _n1, double _v); // 2-d 
	TMultiDimArray(int _n0, int _n1); 
	TMultiDimArray(int _n0, int _n1, const TDenseVector &);
	TMultiDimArray(int _n0, int _n1, int _n2, double _v); // 3-d
	TMultiDimArray(int _n0, int _n1, int _n2); 
	TMultiDimArray(int _n0, int _n1, int _n2, const TDenseVector &); 
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, double _v); // 4-d
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3); 
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, const TDenseVector &);
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, double _v); // 5-d
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4); 
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, const TDenseVector &);
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, double _v); // 6-d
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5); 
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, const TDenseVector &);
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6, double _v); // 7-d
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6); 
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6, const TDenseVector &);
	TMultiDimArray(const TMultiDimArray &); 

	// Destruction
	~TMultiDimArray() {}
	
	// Access 
	int Dim() const { return (int)size_along_dim.size(); }
	int Size(int d=0) const; 
	
	// Format
	void Reshape(int _n); 
	void Reshape(int _n0, int _n1); 
	void Reshape(int _n0, int _n1, int _n2); 
	void Reshape(int _n0, int _n1, int _n2, int _n3); 
	void Reshape(int _n0, int _n1, int _n2, int _n3, int _n4); 
	void Reshape(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5); 
	void Reshape(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6); 
	void Reshape(const std::vector<int> & _size_spec);

	// With 1 index
	double operator()(int, bool) const; // (i)
	TMultiDimArray operator() (int) const; // (i,:,...)
	TMultiDimArray operator() (const TIndex &) const; // (i1:i2,:,...)
	
	void Set(double v, int i); // (i) = v
	void Set(const TMultiDimArray &, int); // (i,:,...) = v	
	void Set(const TMultiDimArray &, const TIndex &); // (i1:i2,:,...) = v

	// With 2 indices
	double operator()(int, int, bool) const; // (i,j) 
	TMultiDimArray operator()(int, int) const; // (i,j,:,...)
	TMultiDimArray operator()(const TIndex &, const TIndex &) const ; // (i1:i2,j1:j2,:,...)
	TMultiDimArray operator()(const TIndex &, int) const; // (i1:,i2, j,:,...)
	TMultiDimArray operator()(int, const TIndex &) const; // (i, j1:j2,:,...)

	void Set(double v, int, int); // (i) = v
	void Set(const TMultiDimArray &v, int I, int J); // (i,j,:,...) = v
	void Set(const TMultiDimArray &v, const TIndex &, const TIndex &); // (i1:i2,j1:j2,:,...) = v
	void Set(const TMultiDimArray &v, const TIndex &, int); // (i1:i2,j,:,...) = v
	void Set(const TMultiDimArray &v, int, const TIndex &); // (i,j1:j2,:,...) = v

	// With 3 indices
	double operator()(int, int, int, bool) const; // (i,j,k)
	TMultiDimArray operator()(int, int, int) const; // (i,j,k,:,...)
	TMultiDimArray operator()(const TIndex &, int, int) const; // (i1:i2,j,k,:,...)
	TMultiDimArray operator()(int, const TIndex &, int) const; // (i,j1:j2,k,:,...)
	TMultiDimArray operator()(int, int, const TIndex &) const; // (i,j,k1:k2,:,...)
	TMultiDimArray operator()(const TIndex &, const TIndex &, int) const; // (i1:i2,j1:j2,k,:,...)
	TMultiDimArray operator()(const TIndex &, int, const TIndex &) const; // (i1:i2,j,k1:k2,:,...)
	TMultiDimArray operator()(int, const TIndex &, const TIndex &) const; // (i,j1:j2,k1:k2,:,...)	
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &) const; // (i1:i2,j1:j2,k1:k2,...)
	
	void Set(double , int I, int J, int K); // (i,j,k) = v
	void Set(const TMultiDimArray &v, int, int , int); // (i,j,k, :,...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, int J, int K); // (i1:i2,j,k, :,...) = v
	void Set(const TMultiDimArray &v, int I, const TIndex &J, int K); // (i,j1:j2,k, :,...) = v
	void Set(const TMultiDimArray &v, int I, int J, const TIndex &K); // (i,j,k1:k2, :,...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J, int K); // (i1:i2,j1:j2,k, :, ...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, int J, const TIndex &K);  // (i1:i2,j,k1:k2, :,...)=v
	void Set(const TMultiDimArray &v, int I, const TIndex &J, const TIndex &K); //(i,j1:j2,k1:k2, :, ...) = v
	void Set(const TMultiDimArray &v, const TIndex &, const TIndex &, const TIndex &); // (i1:i2,j1:j2,k1:k2, :, ...) = v 

	// With 4 indices
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;  // (i1:i2, j1:j2, k1:k2, l1:l2, :, ...)
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, int) const; // (i1:i2,j1:j2,k1:k2,l,:,...)
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, const TIndex &) const; // (i1:i2,j1:j2,k,l1:l2,:,...)
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, const TIndex &) const; // (i1:i2, j, k1:k2, l1:l2,:,...)
	TMultiDimArray operator()(int, const TIndex &, const TIndex &, const TIndex &) const; // (i,j1:j2,k1:k2,l1:l2,...)
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, int) const; // (i1:i2,j1:j2,k,l,:,...) 
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, int) const; // (i1:i2,j,k1:k2,l,:,...)
	TMultiDimArray operator()(const TIndex &, int, int, const TIndex &) const; // (i1:i2,j,k,l1:l2,:,...)
	TMultiDimArray operator()(int, const TIndex &, const TIndex &, int) const; // (i,j1:j2,k1:k2,l,:,...)
	TMultiDimArray operator()(int, const TIndex &, int, const TIndex &) const; // (i,j1:j2,k,l1:l2,:,...)
	TMultiDimArray operator()(int, int, const TIndex &, const TIndex &) const; // (i,j,k1:k2,l1:l2,:,...)
	TMultiDimArray operator()(int, int, int, const TIndex &) const;	//(i,j,k,l1:l2,:,...)
	TMultiDimArray operator()(int, int, const TIndex &, int) const; //(i,j,k1:k2,l,:,...)
	TMultiDimArray operator()(int, const TIndex &, int, int) const; //(i,j1:j2,k,l,:,...)
	TMultiDimArray operator()(const TIndex &, int, int, int) const; //(i1:i2,j,k,l,:,...)
	TMultiDimArray operator()(int, int, int, int) const; // (i,j,k,l,:,...)
	double operator()(int, int, int, int, bool) const; // (i,j,k,l)

	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &); 
// (i1:i2, j1:j2, k1:k2, l1:l2, :, ...) = v
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, int K); // (i1:i2,j1:j2,k1:k2,l,:,...) = v
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, int, const TIndex &); // (i1:i2, j1:j2, k1:k2, l1:l2, :, ...) = v
	void Set(const TMultiDimArray &, const TIndex &, int, const TIndex &, const TIndex &); // (i1:i2,j,k1:k2,l1:l2,:,...) = v
	void Set(const TMultiDimArray &, int, const TIndex &, const TIndex &, const TIndex &); // (i,j1:j2,k1:k2,l1:l2,:,...) = v
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, int, int); // (i1:i2,j1:j2,k,l,:,...)=v 
	void Set(const TMultiDimArray &, const TIndex &, int, const TIndex &, int); // (i1:i2,j,k1:k2,l,:,...)=v
        void Set(const TMultiDimArray &, const TIndex &, int, int, const TIndex &); // (i1:i2,j,k,l1:l2,:,...)=v
        void Set(const TMultiDimArray &, int, const TIndex &, const TIndex &, int); // (i,j1:j2,k1:k2,l,:,...)=v; 
        void Set(const TMultiDimArray &, int, const TIndex &, int, const TIndex &); // (i,j1:j2,k,l1:l2,:,...)=v
        void Set(const TMultiDimArray &, int, int, const TIndex &, const TIndex &); // (i,j,k1:k2,l1:l2,:,...)=v
	void Set(const TMultiDimArray &, int, int, int, const TIndex &); //(i,j,k,l1:l2,:,...) = v
        void Set(const TMultiDimArray &, int, int, const TIndex &, int); //(i,j,k1:k2,l,:,...) = v
        void Set(const TMultiDimArray &, int, const TIndex &, int, int); //(i,j1:j2,k,l,:,...) = v
        void Set(const TMultiDimArray &, const TIndex &, int, int, int); //(i1:i2,j,k,l,:,...) = v
	void Set(const TMultiDimArray &, int, int, int, int); // (i,j,k,l,:,...) = v
	void Set(double, int, int, int, int); // (i,j,k,l) = v
	
	// 5 indices
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
	
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &);  
	
	// 6 indices
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;	
	
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &);

	// 7 indices
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;

        void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &);	

	/*	// Operator
	const TDenseMatrix1D & operator=(const TDenseMatrix1D &right); 
	TDenseMatrix1D operator+(double) const; 
	TDenseMatrix1D operator+(const TDenseMatrix1D &right) const; 
	TDenseMatrix1D operator*(double) const;
        TDenseMatrix1D operator*(const TDenseMatrix1D &right) const;

	// Sum
	double sum(int d) const; 
	double sum() const { return this->sum(0); }

	// Others 
	void Clear(); 
	void Resize(int ); */
}; 
#endif
