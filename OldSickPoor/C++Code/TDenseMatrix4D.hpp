#ifndef _DENSE_MATRIX_4D_
#define _DENSE_MATRIX_4D_

#include <vector>

class TDenseMatrix3D; 
class TDenseMatrix; 
class TDenseVector; 
class TIndex; 

class TDenseMatrix4D : public std::vector<TDenseMatrix3D>
{
public:
	// Construction
	TDenseMatrix4D(int _n1=0, int _n2=0, int _n3=0, int _n4=0, double _v=0.0);
	TDenseMatrix4D(const std::vector<TDenseMatrix3D> &_matrixArray); 
	TDenseMatrix4D(const TDenseMatrix4D &right); 
	// Destruction
	~TDenseMatrix4D(); 

	// RHS Access
	int Size() const { return (int)this->size(); } 
	TDenseMatrix3D operator()(int i) const; 
	TDenseMatrix operator()(int i, int j) const;
	TDenseVector operator()(int i, int j, int k) const; 
	double operator()(int i1, int i2, int i3, int i4) const;

	TDenseMatrix4D operator()(const TIndex &i1, const TIndex &i2, const TIndex &i3, const TIndex &i4) const; // (i1L:i1R,i2L:i2R,i3L:i3R,i4L:i4R)
	TDenseMatrix3D operator()(const TIndex &i1, const TIndex &i2, const TIndex &i3, int i4) const;  // (i1L:i1R,i2L:i2R,i3L:i3R,i4)
	TDenseMatrix3D operator()(const TIndex &i1, const TIndex &i2, int i3, const TIndex &i4) const; // (i1L:i1R,i2L:i2R,i3,i4L:i4R)
	TDenseMatrix3D operator()(const TIndex &i1, int i2, const TIndex &i3, const TIndex &i4) const; // (i1L:i1R,i2,i3L:i3R,i4L:i4R)
	TDenseMatrix3D operator()(int i1, TIndex &i2, const TIndex &i3, const TIndex &i4) const; // (i1,i2L:iLR,i3L:i3R,i4L:i4R)
	
	TDenseMatrix operator()(const TIndex &n1, const TIndex &n2, int n3, int n4) const; // (n1L:n1R, n2L:n2R,n3,n4) 	
	TDenseMatrix operator()(const TIndex &n1, int n2, const TIndex &n3, int n4) const; // (n1L:n1R,n2,n3L:n3R,n4)
	TDenseMatrix operator()(const TIndex &n1, int n2, int n3, const TIndex &n4) const; // (n1L:n1R,n2,n3,n4L:n4R)
	TDenseMatrix operator()(int n1, const TIndex &n2, const TIndex &n3, int n4) const; // (n1,n2L:n2R,n3L:n3R,n4)
	TDenseMatrix operator()(int n1, const TIndex &n2, int n3, const TIndex &n4) const; // (n1,n2L:n2R,n3,n4L:n4R)
	TDenseMatrix operator()(int n1, int n2, const TIndex &n3, const TIndex &n4) const; // (n1,n2,n3L:n3R,n4L:n4R)

	TDenseVector operator()(const TIndex &n1, int n2, int n3, int n4) const; // (n1L:n1R,n2,n3,n4)
	TDenseVector operator()(int n1, const TIndex &n2, int n3, int n4) const; // (n1, n2L:n2R, n3, n4)
	TDenseVector operator()(int n1, int n2, const TIndex &n3, int n4) const; // (n1, n2, n3L:n3R, n4)
	TDenseVector operator()(int n1, int n2, int n3, const TIndex &n4) const; // (n1, n2, n3, n4L:n4R)

	// Set values
	void Set(double v, int n1, int n2, int n3, int n4); // (n1,n2,n3,n4)=v;
	void Set(const TDenseVector &v, int n1, int n2, int n3); //(n1,n2,n3,:) =v;
	void Set(const TDenseMatrix &v, int n1, int n2); // (n1,n2,:,:)=v;
	void Set(const TDenseMatrix3D &v, int n1); // (n1,:,:,:)=v
	
	void Set(const TDenseMatrix4D &v, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4); // (n1L,n2R,n2L:n2R,n3L:n3R,n4L:n4R) = v
	void Set(const TDenseMatrix3D &v, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4); // (n1L:n1R,,n2L:n2R,n3L:n3R,n4) = v
	void Set(const TDenseMatrix3D &v, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4); // (n1L:n1R,n2L:n2R,n3,n4L:n4R)
	void Set(const TDenseMatrix3D &v, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4); // (n1L:n1R,n2,n3L:n3R,n4L:n4R) = v
	void Set(const TDenseMatrix3D &v, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4); // (n1, n2L:n2R,n3L:n3R,n4L:n4R) = v

	void Set(const TDenseMatrix &v, const TIndex &n1, const TIndex &n2, int n3, int n4); // (n1L:n1R, n2L:n2R,n3,n4) =v
	void Set(const TDenseMatrix &v, const TIndex &n1, int n2, const TIndex &n3, int n4); // (n1L:n1R, n2, n3L:n3R, n4) =v
	void Set(const TDenseMatrix &v, const TIndex &n1, int n2, int n3, const TIndex &n4); // (n1L:n1R, n2, n3, n4L:n4R) =v 
	void Set(const TDenseMatrix &v, int n1, const TIndex &n2, const TIndex &n3, int n4); // (n1,n2L:n2R,n3L:n3R,n4)=v; 
	void Set(const TDenseMatrix &v, int n1, const TIndex &n2, int n3, const TIndex &n4); // (n1,n2L:n2R,n3,n4L:n4R)=v
	void Set(const TDenseMatrix &v, int n1, int n2, const TIndex &n3, const TIndex &n4); // (n1,n2,n3L:n3R,n4L:n4R)=v
	void Set(const TDenseVector &v, const TIndex &n1, int n2, int n3, int n4); // (n1L:n1R,n2,n3,n4) = v
	void Set(const TDenseVector &v, int n1, const TIndex &n2, int n3, int n4); // (n1,n2L:n2R,n3,n4) = v
	void Set(const TDenseVector &v, int n1, int n2, const TIndex &n3, int n4); // (n1,n2,n3L:n3R,n4) = v
	void Set(const TDenseVector &v, int n1, int n2, int n3, const TIndex &n4); // (n1,n2,n3,n4L:n4R) = v

	// Operators
	const TDenseMatrix4D & operator=(const TDenseMatrix4D &right); 
	TDenseMatrix4D operator+(double ) const ;
        TDenseMatrix4D operator+(const TDenseMatrix4D &right) const;
        TDenseMatrix4D operator*(double ) const;
        TDenseMatrix4D operator*(const TDenseMatrix4D &right) const;

        // Sum
        TDenseMatrix3D sum(int d) const;
        TDenseMatrix sum(int i, int j) const;
	TDenseVector sum(int i, int j, int k) const; 
        double sum() const;

        // others
        void Clear();
        void Resize(int, int, int, int);
};

#endif
