#ifndef _MULTI_DIMENSIONAL_ARRAY_HEADER_
#define _MULTI_DIMENSIONAL_ARRAY_HEADER_

#include <vector>
class TDenseVector; 
class TIndex; 
const bool SCALAR = true; 

// TMultiDimArray are column major arranged. 
// Given a TMultiDimArray of n0 by n1 by n2, then the corresponding TDenseVector stores
// 1st n0 elements, 2nd n0 element, ...., n1*n2-th n0 element

class TMultiDimArray : public TDenseVector 
{
private: 
	std::vector<int> size_along_dim; 
	bool out_of_range(const TIndex &, int) const; 
	bool out_of_range(int, int) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &) const ; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &) const ; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
	TMultiDimArray ExtractSubMatrix(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
	TMultiDimArray ExtractSubMatrix(const std::vector<TIndex> &) const; 

public: 
	// ******************* Construction *************************
	// 1-d array of size _n, all elements initialized to _v
	TMultiDimArray(int _n, double _v);
	// 1-d array of size _n, uninitialized
	TMultiDimArray(int _n); 
	// 1-d array constructed on TDenseVector object, so that array size and content are the same as the TDenseVector object
	TMultiDimArray(const TDenseVector &); 
	// 1-d array of size _n. If _n < TDenseVector object's dimension, then initialize the array using the first _n elements of the TDenseVector
	// object; otherwise, initialize the first few elements of the array using the TDenseVector object and leave the remaining elements 
	// uninitialized 
	TMultiDimArray(int _n, const TDenseVector &); 

	// 2-d array of _n0 x _n1, all elements initialized to _v
	TMultiDimArray(int _n0, int _n1, double _v);
	// 2-d array of _n0 * _n1, uninitialized 
	TMultiDimArray(int _n0, int _n1); 
	// 2-d array of _n0 x _n1. If _n0 x _n1 < TDenseVector object's dimension, then initialize the array using the first _n0 x _n1 elements of
	// the TDenseVector object; otherwise, initialize the first few elements of the array using the TDenseVector object and leave the remaining
	// elements uninitialized.
	TMultiDimArray(int _n0, int _n1, const TDenseVector &);

	// 3-d array of _n0 x _n1 x _n2, all elements initialized to _v
	TMultiDimArray(int _n0, int _n1, int _n2, double _v);
	// 3-d array of _n0 x _n1 x _n2, uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2); 
	// 3-d array of _n0 x _n1 x _n2. If _n0 x _n1 x _n2 < TDenseVector object's dimension, then initialize the array using the first _n0 x _n1 
	// x _n2 elements of the TDenseVector object; otherwise, initialize the first few elements of the array using the TDenseVector object and 
	// leave the remaining elements uninitialized.
	TMultiDimArray(int _n0, int _n1, int _n2, const TDenseVector &); 

	// 4-d array of _n0 x _n1 x _n2 x _n3, all elements initialized to _v
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, double _v); 
	// 4-d array of _n0 x _n1 x _n2 x _n3, uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3); 
	// 4-d array of _n0 x _n1 x _n2 x _n3. If _n0 x _n1 x _n2 x _n3 < TDenseVector object's dimension, then initialize the array using the first 
	// _n0 x _n1 x _n2 x _n3 elements of the TDenseVector object; otherwise, initialize the first few elements of the array using TDenseVector
	// object and leave the remaining elements uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, const TDenseVector &);

	// 5-d array of _n0 x _n1 x _n2 x _n3 x _n4, all elements initialized to _v
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, double _v); 
	// 5-d array of _n0 x _n1 x _n2 x _n3 x _n4, uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4); 
	// 5-d array of _n0 x _n1 x _n2 x _n3 x _n4. If _n0 x _n1 x _n2 x _n3 x _n4 < TDenseVector object's dimension, then initialize the array 
	// using the first _n0 x _n1 x _n2 x _n3 x _n4 elements of the TDenseVector object; otherwise, initialize the first few elements of the 
	// array using TDenseVector object and leave the remaining elements uninitialized 
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, const TDenseVector &);

	// 6-d array of _n0 x _n1 x _n2 x _n3 x _n4 x _n5, all elements initialized to _v
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, double _v);
	// 6-d array of _n0 x _n1 x _n2 x _n3 x _n4 x _n5, uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5); 
	// 6-d array of _n0 x _n1 x _n2 x _n3 x _n4 x _n5. If _n0 x _n1 x _n2 x _n3 x _n4 x _n5 < TDenseVector object's dimension, then initialize
	// the array using the first _n0 x _n1 x _n2 x _n3 x _n4 x _n5 elements of the TDenseVector object; otherwise, initialize the first few 
	// elements of the array using TDenseVector object and leave the remaining elements uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, const TDenseVector &);

	// 7-d array of _n0 x _n1 x _n2 x _n3 x _n4 x _n5 x _n6, all elements initialized to _v
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6, double _v);
	// 7-d array of _n0 x _n1 x _n2 x _n3 x _n4 x _n5 x _n6, uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6); 
	// 7-d array of _n0 x _n1 x _n2 x _n3 x _n4 x _n5 x _n6. If _n0 x _n1 x _n2 x _n3 x _n4 x _n5 x _n6 < TDenseVector object's dimension, then
	// initialize the array using the first _n0 x _n1 x _n2 x _n3 x _n4 x _n5 x _n6 elements of the TDenseVector object; otherwise, initialize
	// the first few elements of the array using TDenseVector object and leave the remaining elements uninitialized
	TMultiDimArray(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6, const TDenseVector &);

	// Up to 7 dimensional array
	TMultiDimArray(const std::vector<int> &_n, double _v); 
	TMultiDimArray(const std::vector<int> &_n); 
	TMultiDimArray(const std::vector<int> &_n, const TDenseVector &); 
	// Copy constructor
	TMultiDimArray(const TMultiDimArray &); 

	// *********************** Destruction **************************
	~TMultiDimArray() {}
	
	// *********************** Access *******************************
	// Number of dimensions
	int Dim() const; // { return (int)size_along_dim.size(); }
	// Size along the d-th dimension
	int Size(int d=0) const; 
	
	// *********************** Format *******************************
	// Reshape the original array according to the specified sizes, where the desired sizes can be specified using integers, or a vector
	// of integers.  
	// If the size (or the product of the sizes along all the dimensions) of the original array < the specified size (or the product of the
	// specified sizes along all the dimensions), then all the elements of the original array are retained; otherwise, only the first few 
	// elements of the original array are retained, while the remaining elements are disregarded.
	void Reshape(int _n); 
	void Reshape(int _n0, int _n1); 
	void Reshape(int _n0, int _n1, int _n2); 
	void Reshape(int _n0, int _n1, int _n2, int _n3); 
	void Reshape(int _n0, int _n1, int _n2, int _n3, int _n4); 
	void Reshape(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5); 
	void Reshape(int _n0, int _n1, int _n2, int _n3, int _n4, int _n5, int _n6); 
	void Reshape(const std::vector<int> & _size_spec);

	// ************************ Get/Set values ************************
	// Get/set values with 1 index, where the index can be an integer or an TIndex object.
	
	// Can only be applied to 1-dimensional array
	double operator()(int, bool) const; // (i), where the 2nd parameter, bool type, must be set as SCALAR
	void Set(double v, int i); // (i) = v
	
	// Can be applied to 1- or multi-dimensional array
	TMultiDimArray operator() (int) const; // (i,:,...)
	void Set(const TMultiDimArray &v, int INDEX) { Set(v,TIndex(INDEX)) ; } // (i,:,...) = v	

	// TIndex can be used for (1) one index as an integer, e.g., TIndex(1), (2) many indices as a vector of integers, e.g., TIndex(1)(2)(5)(8),
	// (3) a conseutive range of indices, e.g, TIndex(0, 5), or (4) a range of indices with increment, e.g, TIndex(0,2,8)
	TMultiDimArray operator() (const TIndex &) const; // (i1:i2,:,...)
	void Set(const TMultiDimArray &, const TIndex &); // (i1:i2,:,...) = v

	// Get/set values with 2 indices, where the indices can be integers, or TIndex objects, or their combinations
	// When a TIndex object is used to specify indices, it may represent an integer, multiple integers, a consecutive range of integers, or 
	// an incremental range of integers  
	
	// Can only be applied to 2-dimensional array 
	double operator()(int, int, bool) const; // (i,j), where the 3rd parameter, bool type, must be set as SCALAR
	void Set(double v, int, int); // (i, j) = v
	
	TMultiDimArray operator()(int, int) const; // (i,j,:,...)
	void Set(const TMultiDimArray &v, int I, int J) { Set(v, TIndex(I), TIndex(J)); } // (i,j,:,...) = v
	
	TMultiDimArray operator()(const TIndex &, const TIndex &) const ; // (i1:i2,j1:j2,:,...)
	void Set(const TMultiDimArray &v, const TIndex &, const TIndex &); // (i1:i2,j1:j2,:,...) = v
	
	TMultiDimArray operator()(const TIndex &, int) const; // (i1:,i2, j,:,...)
	void Set(const TMultiDimArray &v, const TIndex &I, int J) { Set(v, I, TIndex(J)); } // (i1:i2,j,:,...) = v
	
	TMultiDimArray operator()(int, const TIndex &) const; // (i, j1:j2,:,...)
	void Set(const TMultiDimArray &v, int I, const TIndex &J) { Set(v, TIndex(I), J); }  // (i,j1:j2,:,...) = v

	// Get/set values with 3 indices, where the indices can be integers, or TIndex objects, or their combinations
	// When a TIndex object is used to specify indices, it may represent an integer, multiple integers, a consecutive range of integers, or
	// an incremental range of integers
	
	// Can only be applied to 3-dimensional array
	double operator()(int, int, int, bool) const; // (i,j,k), where the 4th parameter, bool type, must be set as SCALAR
	void Set(double , int I, int J, int K); // (i,j,k) = v
	
	TMultiDimArray operator()(int, int, int) const; // (i,j,k,:,...)
	void Set(const TMultiDimArray &v, int I, int J , int K) { Set(v, TIndex(I), TIndex(J), TIndex(K)); } // (i,j,k, :,...) = v
	
	TMultiDimArray operator()(const TIndex &, int, int) const; // (i1:i2,j,k,:,...)
	void Set(const TMultiDimArray &v, const TIndex &I, int J, int K) { Set(v, I, TIndex(J), TIndex(K)); } // (i1:i2,j,k, :,...) = v
	
	TMultiDimArray operator()(int, const TIndex &, int) const; // (i,j1:j2,k,:,...)
	void Set(const TMultiDimArray &v, int I, const TIndex &J, int K) { Set(v, TIndex(I), J, TIndex(K)); }// (i,j1:j2,k, :,...) = v
	
	TMultiDimArray operator()(int, int, const TIndex &) const; // (i,j,k1:k2,:,...)
	void Set(const TMultiDimArray &v, int I, int J, const TIndex &K) { Set(v, TIndex(I), TIndex(J), K); } // (i,j,k1:k2, :,...) = v
	
	TMultiDimArray operator()(const TIndex &, const TIndex &, int) const; // (i1:i2,j1:j2,k,:,...)
	void Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J, int K) { Set(v, I, J, TIndex(K)); } // (i1:i2,j1:j2,k, :, ...) = v
	
	TMultiDimArray operator()(const TIndex &, int, const TIndex &) const; // (i1:i2,j,k1:k2,:,...)
	void Set(const TMultiDimArray &v, const TIndex &I, int J, const TIndex &K) { Set(v, I, TIndex(J), K); }  // (i1:i2,j,k1:k2, :,...)=v
	
	TMultiDimArray operator()(int, const TIndex &, const TIndex &) const; // (i,j1:j2,k1:k2,:,...)	
	void Set(const TMultiDimArray &v, int I, const TIndex &J, const TIndex &K) { Set(v, TIndex(I), J, K); }//(i,j1:j2,k1:k2, :, ...) = v
	
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &) const; // (i1:i2,j1:j2,k1:k2,...)
	void Set(const TMultiDimArray &v, const TIndex &, const TIndex &, const TIndex &); // (i1:i2,j1:j2,k1:k2, :, ...) = v 

	// Get/set values with 4 indices, where the indices can be integers, or TIndex objects, or their combinations
	// When a TIndex object is used to specify indices, it may represent an integer, multiple integers, a consecutive range of integers, or
	// an incremental range of integers
	
	// Can only be applied to 4-dimensional array 
	double operator()(int, int, int, int, bool) const; // (i,j,k,l)
	void Set(double, int, int, int, int); // (i,j,k,l) = v
		
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;  // (i1:i2, j1:j2, k1:k2, l1:l2, :, ...)
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &); // (i1:i2, j1:j2, k1:k2, l1:l2, :, ...) = v
	
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, int) const; // (i1:i2,j1:j2,k1:k2,l,:,...)
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3) { Set(v,n0,n1,n2,TIndex(n3)); } // (i1:i2,j1:j2,k1:k2,l,:,...) = v
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, const TIndex &) const; // (i1:i2,j1:j2,k,l1:l2,:,...)
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3) { Set(v,n0,n1,TIndex(n2),n3); } // (i1:i2,j1:j2,k,l1:l2,:,...) = v
	
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, const TIndex &) const; // (i1:i2,j,k1:k2,l1:l2,:,...)
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3) { Set(v,n0,TIndex(n1),n2,n3); } // (i1:i2,j,k1:k2,l1:l2,:,...) = v

	TMultiDimArray operator()(int, const TIndex &, const TIndex &, const TIndex &) const; // (i,j1:j2,k1:k2,l1:l2,...)
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3) { Set(v,TIndex(n0),n1,n2,n3); } // (i,j1:j2,k1:k2,l1:l2,:,...) = v
	
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, int) const; // (i1:i2,j1:j2,k,l,:,...) 
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3) { Set(v, n0, n1, TIndex(n2), TIndex(n3)); } // (i1:i2,j1:j2,k,l,:,...) = v 
	
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, int) const; // (i1:i2,j,k1:k2,l,:,...)
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3) { Set(v, n0, TIndex(n1), n2, TIndex(n2)); } // (i1:i2,j,k1:k2,l,:,...) = v

	TMultiDimArray operator()(const TIndex &, int, int, const TIndex &) const; // (i1:i2,j,k,l1:l2,:,...)
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3) { Set(v, n0, TIndex(n1), TIndex(n2), n3); } // (i1:i2,j,k,l1:l2,:,...)=v

	TMultiDimArray operator()(int, const TIndex &, const TIndex &, int) const; // (i,j1:j2,k1:k2,l,:,...)
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3) { Set(v, TIndex(n0), n1, n2, TIndex(n3)); } // (i,j1:j2,k1:k2,l,:,...)=v; 

	TMultiDimArray operator()(int, const TIndex &, int, const TIndex &) const; // (i,j1:j2,k,l1:l2,:,...)
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3) { Set(v, TIndex(n0), n1, TIndex(n2), n3); } // (i,j1:j2,k,l1:l2,:,...)=v

	TMultiDimArray operator()(int, int, const TIndex &, const TIndex &) const; // (i,j,k1:k2,l1:l2,:,...)
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3) { Set(v, TIndex(n0), TIndex(n1), n2, n3); } // (i,j,k1:k2,l1:l2,:,...)=v

	TMultiDimArray operator()(int, int, int, const TIndex &) const;	//(i,j,k,l1:l2,:,...)
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3) { Set(v, TIndex(n0), TIndex(n1), TIndex(n2), n3); }  // (i,j,k,l1:l2,:,...) = v

	TMultiDimArray operator()(int, int, const TIndex &, int) const; //(i,j,k1:k2,l,:,...)
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3) { Set(v, TIndex(n0), TIndex(n1), n2, TIndex(n3)); } // (i,j,k1:k2,l,:,...) = v

	TMultiDimArray operator()(int, const TIndex &, int, int) const; //(i,j1:j2,k,l,:,...)
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3) { Set(v, TIndex(n0), n1, TIndex(n2), TIndex(n3)); } // (i,j1:j2,k,l,:,...) = v

	TMultiDimArray operator()(const TIndex &, int, int, int) const; //(i1:i2,j,k,l,:,...)
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3) { Set(v, n0, TIndex(n1), TIndex(n2), TIndex(n3)); } // (i1:i2,j,k,l,:,...) = v

	TMultiDimArray operator()(int, int, int, int) const; // (i,j,k,l,:,...)
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3) { Set(v, TIndex(n0), TIndex(n1), TIndex(n2), TIndex(n3)); }  // (i,j,k,l,:,...) = v
	
	// Get/set values with 5 indices, where the indices can be integers, or TIndex objects, or their combinations
	// When a TIndex object is used to specify indices, it may represent an integer, multiple integers, a consecutive range of integers, or
	// an incremental range of integers 
	
 	// Can only be applied to 5-dimensional array
	double operator()(int, int, int, int, int, bool) const; 
	void Set(double, int, int, int, int, int); 
	
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
	TMultiDimArray operator()(int, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const; 
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, const TIndex &, const TIndex &) const;
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, const TIndex &, const TIndex &) const;
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, int, const TIndex &) const;
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, int) const;
	
	TMultiDimArray operator()(int, int, const TIndex &, const TIndex &, const TIndex &) const;
	TMultiDimArray operator()(int, const TIndex &, int, const TIndex &, const TIndex &) const;
	TMultiDimArray operator()(int, const TIndex &, const TIndex &, int, const TIndex &) const;
	TMultiDimArray operator()(int, const TIndex &, const TIndex &, const TIndex &, int) const;
	TMultiDimArray operator()(const TIndex &, int, int, const TIndex &, const TIndex &) const;
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, int, const TIndex &) const;
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, const TIndex &, int) const;
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, int, const TIndex &) const;
	TMultiDimArray operator()(const TIndex &, const TIndex &, int, const TIndex &, int) const;
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, int, int) const;

	TMultiDimArray operator()(const TIndex &, const TIndex &, int, int, int) const;
	TMultiDimArray operator()(const TIndex &, int, const TIndex &, int, int) const;
	TMultiDimArray operator()(const TIndex &, int, int, const TIndex &, int) const;
	TMultiDimArray operator()(const TIndex &, int, int, int, const TIndex &) const;
	TMultiDimArray operator()(int, const TIndex &, const TIndex &, int, int) const;
	TMultiDimArray operator()(int, const TIndex &, int, const TIndex &, int) const;
	TMultiDimArray operator()(int, const TIndex &, int, int, const TIndex &) const;
	TMultiDimArray operator()(int, int, const TIndex &, const TIndex &, int) const;
	TMultiDimArray operator()(int, int, const TIndex &, int, const TIndex &) const;
	TMultiDimArray operator()(int, int, int, const TIndex &, const TIndex &) const;

	TMultiDimArray operator()(int, int, int, int, const TIndex &) const; 
	TMultiDimArray operator()(int, int, int, const TIndex &, int) const;
	TMultiDimArray operator()(int, int, const TIndex &, int, int) const;
	TMultiDimArray operator()(int, const TIndex &, int, int, int) const;
	TMultiDimArray operator()(const TIndex &, int, int, int, int) const;
	
	TMultiDimArray operator()(int, int, int, int, int) const; 


	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &);  
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) { Set(v,TIndex(n0),n1,n2,n3,n4); } 
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) { Set(v,n0,TIndex(n1),n2,n3,n4); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4) { Set(v,n0,n1,TIndex(n2),n3,n4); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4) { Set(v,n0,n1,n2,TIndex(n3),n4); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4) { Set(v,n0,n1,n2,n3,TIndex(n4)); }

	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4) { Set(v,n0,n1,TIndex(n2),n4,TIndex(n4)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4)); }

	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, int n4) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, int n4) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4)); } 
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, int n4) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, const TIndex &n4) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, int n4) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, int n4) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, const TIndex &n4) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, int n4) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, const TIndex &n4) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, const TIndex &n4) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4); }

	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex &n4) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, int n4) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,TIndex(n4)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, int n4) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, int n4) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, int n4) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4)); }

        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4)); }

	// Get/set values with 6 indices, where the indices can be integers, or TIndex objects, or their combinations
	// When a TIndex object is used to specify indices, it may represent an integer, multiple integers, a consecutive range of integers, or
	// an incremental range of integers	
	
	// Can only be applied to 6-dimensional array
	double operator()(int n0, int n1, int n2, int n3, int n4, int n5, bool flag_scalar) const; 
	void Set(double v, int n0, int n1, int n2, int n3, int n4, int n5); 
	
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) const; 
	
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5) const; 
	
	TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) const;
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5) const; 

	TMultiDimArray operator()(const TIndex&n0, const TIndex&n1, int n2, int n3, int n4, int n5) const;
        TMultiDimArray operator()(const TIndex&n0, int n1, const TIndex&n2, int n3, int n4, int n5) const; 
        TMultiDimArray operator()(const TIndex&n0, int n1, int n2, const TIndex&n3, int n4, int n5) const; 
        TMultiDimArray operator()(const TIndex&n0, int n1, int n2, int n3, const TIndex&n4, int n5) const; 
        TMultiDimArray operator()(const TIndex&n0, int n1, int n2, int n3, int n4, const TIndex&n5) const; 
        TMultiDimArray operator()(int n0, const TIndex&n1, const TIndex&n2, int n3, int n4, int n5) const; 
        TMultiDimArray operator()(int n0, const TIndex&n1, int n2, const TIndex&n3, int n4, int n5) const; 
        TMultiDimArray operator()(int n0, const TIndex&n1, int n2, int n3, const TIndex&n4, int n5) const; 
        TMultiDimArray operator()(int n0, const TIndex&n1, int n2, int n3, int n4, const TIndex&n5) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex&n2, const TIndex&n3, int n4, int n5) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex&n2, int n3, const TIndex&n4, int n5) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex&n2, int n3, int n4, const TIndex&n5) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex&n3, const TIndex&n4, int n5) const;
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex&n3, int n4, const TIndex&n5) const;
        TMultiDimArray operator()(int n0, int n1, int n2, int n3, const TIndex&n4, const TIndex&n5) const;
	
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, int n4, int n5) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, int n4, int n5) const;
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, int n4, int n5) const;
	TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, int n4, int n5) const;
	TMultiDimArray operator()(int n0, int n1, int n2, int n3, const TIndex &n4, int n5) const;
	TMultiDimArray operator()(int n0, int n1, int n2, int n3, int n4, const TIndex &n5) const;
	
	TMultiDimArray operator()(int n0, int n1, int n2, int n3, int n4, int n5) const;
	
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5);

	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),n1,n2,n3,n4,n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,TIndex(n1),n2,n3,n4,n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,n1,TIndex(n2),n3,n4,n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,n1,n2,TIndex(n3),n4,n4); } 
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,n0,n1,n2,n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,n0,n1,n2,n3,n4,TIndex(n5)); }
	
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4,n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4,n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4,n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,TIndex(n0),n1,n2,n3,n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4,n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4,n4); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,n0,TIndex(n1),n2,n3,n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4,n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,n0,n1,TIndex(n2),n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,n0,n1,TIndex(n2),n3,n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5) { Set(v,n0,n1,n2,TIndex(n3),n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5) { Set(v,n0,n1,n2,n3,TIndex(n4),TIndex(n5)); }

	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4,n5); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),n4,n5); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4,n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4,n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4),n5); } 
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4),n5); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5) { Set(v,n0,n1,TIndex(n2),n3,TIndex(n4),TIndex(n5)); }

	void Set(const TMultiDimArray &v, const TIndex&n0, const TIndex&n1, int n2, int n3, int n4, int n5) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5)); }
        void Set(const TMultiDimArray &v, const TIndex&n0, int n1, const TIndex&n2, int n3, int n4, int n5) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4),TIndex(n5)); }
        void Set(const TMultiDimArray &v, const TIndex&n0, int n1, int n2, const TIndex&n3, int n4, int n5) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4),TIndex(n5)); }
        void Set(const TMultiDimArray &v, const TIndex&n0, int n1, int n2, int n3, const TIndex&n4, int n5) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4,TIndex(n5)); }
        void Set(const TMultiDimArray &v, const TIndex&n0, int n1, int n2, int n3, int n4, const TIndex&n5) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),n5); }
        void Set(const TMultiDimArray &v, int n0, const TIndex&n1, const TIndex&n2, int n3, int n4, int n5) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4),TIndex(n5)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex&n1, int n2, const TIndex&n3, int n4, int n5) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4),TIndex(n5)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex&n1, int n2, int n3, const TIndex&n4, int n5) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4,TIndex(n5)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex&n1, int n2, int n3, int n4, const TIndex&n5) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4),n5); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex&n2, const TIndex&n3, int n4, int n5) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4),TIndex(n5)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex&n2, int n3, const TIndex&n4, int n5) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n4),n4,TIndex(n5)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex&n2, int n3, int n4, const TIndex&n5) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4),n5); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex&n3, const TIndex&n4, int n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4,TIndex(n5)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex&n3, int n4, const TIndex&n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n2,TIndex(n4),n5); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex&n4, const TIndex&n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4,n5); }

	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, int n4, int n5) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, int n4, int n5) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, int n4, int n5) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, int n4, int n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,TIndex(n4),TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex &n4, int n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4,TIndex(n5)); }
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4, const TIndex &n5) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),n5); }

	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4, int n5) { Set(v, TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5)); }
	
	// Get/set values with 7 indices, where the indices can be integers, or TIndex objects, or their combinations
	// When a TIndex object is used to specify indices, it may represent an integer, multiple integers, a consecutive range of integers, or
	// an incremental range of integers
	
	// Can only be applied to 7-dimensional array
	double operator()(int n0, int n1, int n2, int n3, int n4, int n5, int n6, bool scalar_flag) const; 
	void Set(double v, int n0, int n1, int n2, int n3, int n4, int n5, int n6);
	
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const;
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const;
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const;
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const;
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const;
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 

	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) const;
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const;  
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const;
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const;  
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const;  
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const;  
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const;
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
	
	TMultiDimArray operator()(int n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, int n5, const TIndex &n6) const; 
	TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, int n6) const;  
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const;  
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const;  
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, int n6) const;  
	TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, int n6) const;  
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, int n6) const; 

        TMultiDimArray operator()(int n0, int n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) const;
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, int n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, int n4, int n5, const TIndex &n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, int n4, const TIndex &n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, int n5, int n6) const; 
	
        TMultiDimArray operator()(const TIndex &n0, int n1, int n2, int n3, int n4, int n5, int n6) const;
        TMultiDimArray operator()(int n0, const TIndex &n1, int n2, int n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, const TIndex &n2, int n3, int n4, int n5, int n6) const; 
        TMultiDimArray operator()(int n0, int n1, int n2, const TIndex &n3, int n4, int n5, int n6) const;
        TMultiDimArray operator()(int n0, int n1, int n2, int n3, const TIndex &n4, int n5, int n6) const;
        TMultiDimArray operator()(int n0, int n1, int n2, int n3, int n4, const TIndex &n5, int n6) const;
        TMultiDimArray operator()(int n0, int n1, int n2, int n3, int n4, int n5, const TIndex &n6) const; 
	
	TMultiDimArray operator()(int n0, int n1, int n2, int n3, int n4, int n5, int n6) const; 
	
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &);
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,n1,n2,n3,n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,n1,n2,n3,n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,n2,n3,TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,n2,TIndex(n3),n4,n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),n3,n4,n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,n3,n4,n5,n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,n3,n4,n5,n6); }
	
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,n0,n1,n2,n3,n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,n0,n1,n2,n3,TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,n1,n2,TIndex(n3),n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,n1,TIndex(n2),n3,n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),n2,n3,n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,n2,n3,n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,n0,n1,n2,n3,TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,n1,n2,TIndex(n3),n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),n3,n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,n3,n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,n3,n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),n3,TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4,n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4,n5,n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4,n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4,n5,n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4,n5,n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4,n5,n6); }
        
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,n0,n1,n2,n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,n0,n1,n2,TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,n0,n1,TIndex(n2),n3,n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,n0,TIndex(n1),n2,n3,n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),n1,n2,n3,n4,TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,n0,n1,TIndex(n2),n3,TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),n3,TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4),n5,n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4),n5,n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4),n5,n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4),n5,n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4,n5,n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4,n5,n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),n4,n5,n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4,n5,n6); }
	
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4,n5,n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),n5,n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4,TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),n6); }
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4,n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,n4,TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,TIndex(n0),n1,n2,n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,n0,TIndex(n1),n2,n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,n0,n1,TIndex(n2),n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, int n6) { Set(v,n0,n1,n2,TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }
	
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4, const TIndex &n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),n5,n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex &n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4,TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),n6); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex &n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4,n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3, int n4, int n5, int n6) { Set(v,TIndex(n0),n1,n2,TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3, int n4, int n5, int n6) { Set(v,n0,TIndex(n1),n2,TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3, int n4, int n5, int n6) { Set(v,n0,n1,TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }

        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3, int n4, int n5, int n6) { Set(v,n0,TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3, int n4, int n5, int n6) { Set(v,TIndex(n0),n1,TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3, int n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),n2,TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3, int n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),n3,TIndex(n4),TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, const TIndex &n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),n4,TIndex(n5),TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4, const TIndex &n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),n5,TIndex(n6)); }
        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4, int n5, const TIndex &n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),n6); }

        void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3, int n4, int n5, int n6) { Set(v,TIndex(n0),TIndex(n1),TIndex(n2),TIndex(n3),TIndex(n4),TIndex(n5),TIndex(n6)); }

	// Operator
	const TMultiDimArray & operator=(const TMultiDimArray &right); 
	TMultiDimArray operator+(double) const; 
	const TMultiDimArray& operator+=(double); 
	TMultiDimArray operator+(const TMultiDimArray &right) const; 
	const TMultiDimArray& operator+=(const TMultiDimArray &right); 
	TMultiDimArray operator*(double) const;
	const TMultiDimArray& operator*=(double); 
        TMultiDimArray operator*(const TMultiDimArray &right) const;
	const TMultiDimArray& operator*=(const TMultiDimArray &right); 
	TMultiDimArray operator-(double v) const { return operator+(-1.0*v); }
	const TMultiDimArray& operator-=(double v) { return operator+=(-1.0*v); } 
	TMultiDimArray operator-(const TMultiDimArray &right) const { return operator+(right*(-1.0)); }
	const TMultiDimArray& operator-=(const TMultiDimArray &right) { return operator+=(right*(-1.0)); }

	// Sum
	double sum() const; 
	TMultiDimArray sum(int d) const;  
	TMultiDimArray sum(int d0, int d1) const; 
	TMultiDimArray sum(int d0, int d1, int d2) const; 
	TMultiDimArray sum(int d0, int d1, int d2, int d3) const; 
	TMultiDimArray sum(int d0, int d1, int d2, int d3, int d4) const; 
	TMultiDimArray sum(int d0, int d1, int d2, int d3, int d4, int d5) const; 
	TMultiDimArray sum(int d0, int d1, int d2, int d3, int d4, int d5, int d6) const; 
	

	// Others 
	void Clear(); 
}; 
#endif
