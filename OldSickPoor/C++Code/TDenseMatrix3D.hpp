#ifndef _DENSE_MATRIX_3D_ 
#define _DENSE_MATRIX_3D_

#include <vector>

class TDenseVector; 
class TDenseMatrix; 
class TIndex; 

class TDenseMatrix3D : public std::vector<TDenseMatrix>
{
public: 
	// Construction
	TDenseMatrix3D(int _d1=0, int _d2=0, int _d3=0);  
	TDenseMatrix3D(const std::vector<TDenseMatrix> & _matrixArray); 
	TDenseMatrix3D(const TDenseMatrix3D & right); 
	// Destruction
	~TDenseMatrix3D(); 

	// Access
	int Size() const; 
	TDenseMatrix operator()(int i) const; // (i,:,:)
	TDenseVector operator()(int i, int j) const; // (i,j,:)
	double operator()(int i, int j, int k) const; // (i,j,k)
	TDenseMatrix3D operator()(const TIndex &i, const TIndex &j, const TIndex &k) const; // (i1:i2, j1:j2, k1:k2)
	TDenseMatrix operator()(int i, const TIndex &j, const TIndex &k) const; // (i, j1:j2, k1:k2) 
	TDenseMatrix operator()(const TIndex &i, int j, const TIndex &k) const; // (i1:i2, j, k1:k2)
	TDenseMatrix operator()(const TIndex &i, const TIndex &j, int k) const; // (i1:i2, j1:j2, k)

	TDenseVector operator()(int i, const TIndex &j, int k) const; // (i, j1:j2, k)
	TDenseVector operator()(int i, int j, const TIndex &k) const; // (i, j, k1:k2)
	TDenseVector operator()(const TIndex &i, int j, int k) const;  // (i1:i2, j, k)

	void Set(double v, int i, int j, int k);	// (i,j,k) = v 
	void Set(const TDenseVector &v, int i, int j);  // (i,j,:) = v
	void Set(const TDenseMatrix &v, int i); 	// (i,:,:) = v
	void Set(const TDenseMatrix3D &v, const TIndex &i, const TIndex &j, const TIndex &k); //(i1:i2, j1:j2, k1:k2)=v
	void Set(const TDenseMatrix &v, int i, const TIndex &j, const TIndex &k); // (i,j1:j2,k1:k2) = v
	void Set(const TDenseMatrix &v, const TIndex &i, int j, const TIndex &k); // (i1:i2,j,k1:k2) = v
	void Set(const TDenseMatrix &v, const TIndex &i, const TIndex &j, int k); // (i1:i2,j1:j2,k) = v
	void Set(const TDenseVector &v, int i, int j, const TIndex &k); // (i, j, k1:k2) = v 
	void Set(const TDenseVector &v, int i, const TIndex &j, int k); // (i, j1:j2, k) = v
	void Set(const TDenseVector &v, const TIndex &i, int j, int k); // (i1:i2,j,k) = v

	// Operators
	const TDenseMatrix3D & operator=(const TDenseMatrix3D &right); 
	TDenseMatrix3D operator+(double ) const ; 
	TDenseMatrix3D operator+(const TDenseMatrix3D &right) const; 
	TDenseMatrix3D operator*(double ) const;
 	TDenseMatrix3D operator*(const TDenseMatrix3D &right) const; 

	// Sum
	TDenseMatrix sum(int d) const; 
	TDenseVector sum(int i, int j) const; 
	double sum() const; 

	// others
	TDenseMatrix3D & Clear(); 
	TDenseMatrix3D & Resize(int, int, int); 
}; 
#endif
