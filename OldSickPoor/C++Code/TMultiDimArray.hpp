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
	// Construction
	TMultiDimArray(int _n, double _v); // 1-d
	TMultiDimArray(int _n); 
	TMultiDimArray(const TDenseVector &); 
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
	void Set(const TMultiDimArray &, const TIndex &); // (i1:i2,:,...) = v
	void Set(const TMultiDimArray &v, int INDEX) { Set(v,TIndex(INDEX)) ; } // (i,:,...) = v	

	// With 2 indices
	double operator()(int, int, bool) const; // (i,j) 
	TMultiDimArray operator()(int, int) const; // (i,j,:,...)
	TMultiDimArray operator()(const TIndex &, const TIndex &) const ; // (i1:i2,j1:j2,:,...)
	TMultiDimArray operator()(const TIndex &, int) const; // (i1:,i2, j,:,...)
	TMultiDimArray operator()(int, const TIndex &) const; // (i, j1:j2,:,...)

	void Set(double v, int, int); // (i) = v
	void Set(const TMultiDimArray &v, const TIndex &, const TIndex &); // (i1:i2,j1:j2,:,...) = v
	void Set(const TMultiDimArray &v, int I, const TIndex &J) { Set(v, TIndex(I), J); }  // (i,j1:j2,:,...) = v
	void Set(const TMultiDimArray &v, int I, int J) { Set(v, TIndex(I), TIndex(J)); } // (i,j,:,...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, int J) { Set(v, I, TIndex(J)); } // (i1:i2,j,:,...) = v

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
	void Set(const TMultiDimArray &v, const TIndex &, const TIndex &, const TIndex &); // (i1:i2,j1:j2,k1:k2, :, ...) = v 
	void Set(const TMultiDimArray &v, int I, int J , int K) { Set(v, TIndex(I), TIndex(J), TIndex(K)); } // (i,j,k, :,...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, int J, int K) { Set(v, I, TIndex(J), TIndex(K)); } // (i1:i2,j,k, :,...) = v
	void Set(const TMultiDimArray &v, int I, const TIndex &J, int K) { Set(v, TIndex(I), J, TIndex(K)); }// (i,j1:j2,k, :,...) = v
	void Set(const TMultiDimArray &v, int I, int J, const TIndex &K) { Set(v, TIndex(I), TIndex(J), K); } // (i,j,k1:k2, :,...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, const TIndex &J, int K) { Set(v, I, J, TIndex(K)); } // (i1:i2,j1:j2,k, :, ...) = v
	void Set(const TMultiDimArray &v, const TIndex &I, int J, const TIndex &K) { Set(v, I, TIndex(J), K); }  // (i1:i2,j,k1:k2, :,...)=v
	void Set(const TMultiDimArray &v, int I, const TIndex &J, const TIndex &K) { Set(v, TIndex(I), J, K); }//(i,j1:j2,k1:k2, :, ...) = v

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

	void Set(double, int, int, int, int); // (i,j,k,l) = v
	void Set(const TMultiDimArray &, const TIndex &, const TIndex &, const TIndex &, const TIndex &); 
// (i1:i2, j1:j2, k1:k2, l1:l2, :, ...) = v
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, const TIndex &n2, int n3) { Set(v,n0,n1,n2,TIndex(n3)); } // (i1:i2,j1:j2,k1:k2,l,:,...) = v
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, const TIndex &n3) { Set(v,n0,n1,TIndex(n2),n3); } // (i1:i2, j1:j2, k1:k2, l1:l2, :, ...) = v
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, const TIndex &n3) { Set(v,n0,TIndex(n1),n2,n3); } // (i1:i2,j,k1:k2,l1:l2,:,...) = v
	void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, const TIndex &n3) { Set(v,TIndex(n0),n1,n2,n3); } // (i,j1:j2,k1:k2,l1:l2,:,...) = v
	void Set(const TMultiDimArray &v, const TIndex &n0, const TIndex &n1, int n2, int n3) { Set(v, n0, n1, TIndex(n2), TIndex(n3)); } // (i1:i2,j1:j2,k,l,:,...)=v 
	void Set(const TMultiDimArray &v, const TIndex &n0, int n1, const TIndex &n2, int n3) { Set(v, n0, TIndex(n1), n2, TIndex(n2)); } // (i1:i2,j,k1:k2,l,:,...)=v
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, const TIndex &n3) { Set(v, n0, TIndex(n1), TIndex(n2), n3); } // (i1:i2,j,k,l1:l2,:,...)=v
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, const TIndex &n2, int n3) { Set(v, TIndex(n0), n1, n2, TIndex(n3)); } // (i,j1:j2,k1:k2,l,:,...)=v; 
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, const TIndex &n3) { Set(v, TIndex(n0), n1, TIndex(n2), n3); } // (i,j1:j2,k,l1:l2,:,...)=v
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, const TIndex &n3) { Set(v, TIndex(n0), TIndex(n1), n2, n3); }  // (i,j,k1:k2,l1:l2,:,...)=v
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, const TIndex &n3) { Set(v, TIndex(n0), TIndex(n1), TIndex(n2), n3); }  //(i,j,k,l1:l2,:,...) = v
        void Set(const TMultiDimArray &v, int n0, int n1, const TIndex &n2, int n3) { Set(v, TIndex(n0), TIndex(n1), n2, TIndex(n3)); } //(i,j,k1:k2,l,:,...) = v
        void Set(const TMultiDimArray &v, int n0, const TIndex &n1, int n2, int n3) { Set(v, TIndex(n0), n1, TIndex(n2), TIndex(n3)); } //(i,j1:j2,k,l,:,...) = v
        void Set(const TMultiDimArray &v, const TIndex &n0, int n1, int n2, int n3) { Set(v, n0, TIndex(n1), TIndex(n2), TIndex(n3)); } //(i1:i2,j,k,l,:,...) = v
	void Set(const TMultiDimArray &v, int n0, int n1, int n2, int n3) { Set(v, TIndex(n0), TIndex(n1), TIndex(n2), TIndex(n3)); }  // (i,j,k,l,:,...) = v
	
	// 5 indices
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

	double operator()(int, int, int, int, int, bool) const; 

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
	void Set(double, int, int, int, int, int); 

	// 6 indices
	TMultiDimArray operator()(const TIndex &n0, const TIndex &n1, const TIndex &n2, const TIndex &n3, const TIndex &n4, const TIndex &n5) const; 
	double operator()(int n0, int n1, int n2, int n3, int n4, int n5, bool flag_scalar) const; 

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
	
	void Set(double v, int n0, int n1, int n2, int n3, int n4, int n5); 
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
	
	// 7 indices
	TMultiDimArray operator()(const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &, const TIndex &) const;
	double operator()(int n0, int n1, int n2, int n3, int n4, int n5, int n6, bool scalar_flag) const; 

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
	void Set(double v, int n0, int n1, int n2, int n3, int n4, int n5, int n6);

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
