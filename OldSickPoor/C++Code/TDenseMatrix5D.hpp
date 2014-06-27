#include <vector>

class TDenseMatrix4D; 
class TDenseMatrix3D; 
class TDenseMatrix; 
class TDenseVector; 
class TIndex; 

class TDeneMatrix5D : public std::vector<TDenseMatrix4D>
{
public: 
	// Construction
	TDenseMatrix5D(int _n0=0, int _n1=0, int _n2=0, int _n3=0, int _n4=0, double _v=0.0); 
	TDenseMatrix5D(const std::vector<TDenseMatrix4D> &_matrixArray); 
	TDenseMatrix5D(const TDenseMatrix5D &right); 

	// Destruction
	~TDenseMatrix5D(); 
	
	// Size
	int Size() const { return (int)this->size(); }
	int Size(int d) const; 

	// RHS Access
	TDenseMatrix4D operator()(int i) const; 
	TDenseMatrix3D operator()(int i, int j) const; 
	TDenseMatrix operator()(int i, int j, int k) const; 
	TDenseVector operator()(int i0, int i1, int i2, int i3) const; 
	double operator()(int i0, int i1, int i2, int i3, int i4) const; 

	TDenseMatrix5D operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) const;  
};  
