#ifndef _MULTI_DIMENSIONAL_ARRAY_HEADER_
#define _MULTI_DIMENSIONAL_ARRAY_HEADER_

#include <vector>
class TDenseVector; 
class TIndex; 

class TMultiDimArray : public TDenseVector 
{
private: 
	std::vector<int> size_along_dim; 
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

	// With 1 index
	double operator() (int ) const; 
	TMultiDimArray operator() (int, bool) const; // (i,:,...)
	TMultiDimArray operator() (const TIndex &) const; // (i1:i2,:,...)
	
	void Set(double v, int i); 
	void Set(const TMultiDimArray &, int); // (i,:,...) = v	
	void Set(const TMultiDimArray &, const TIndex &); // (i1:i2,:,...) = v

	/*
	TMultiDimArray operator() (const TIndex &i) const; 
	void Set(const TMultiDimArray &right, const TIndex &i); 

	// LHS Access 
	void Set(double v, int i); 
	void Set(const TDenseMatrix1D &, const TIndex &i); 

	// Operator
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
