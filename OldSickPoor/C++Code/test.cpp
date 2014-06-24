#include <iostream>
#include "MultiDimensionArray.hpp"
#include "dw_dense_matrix.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	TDenseMatrix3D matrix3d; 
	matrix3d.Resize(3,4,5); 
	for (int i=0; i<matrix3d.Size(); i++)
	{
		matrix3d.Set(RandomUniform(4,5),i);
		cout << matrix3d(i) << endl; 
	}
	cout << "submatrix " << endl; 
	cout << matrix3d(TIndex(0,2),TIndex(0,1),1) << endl; 
	cout << matrix3d(0,TIndex(0,2),TIndex(0,3)) << endl; 
	cout << matrix3d(TIndex(0,1),2,TIndex(0,3)) << endl; 
	cout << "subvector " << endl; 
	cout << matrix3d(0,2,TIndex(0,2,4)) << endl; 
	cout << matrix3d(TIndex(0,2),2,3) << endl; 
	cout << matrix3d(0,TIndex(1,2,3),4) << endl; 

	matrix3d.Set(RandomUniform(2,3),1,TIndex(0,1),TIndex(2,4)); 
	cout << matrix3d(1,TIndex(0,1),TIndex(2,4)) << endl; 
	matrix3d.Set(RandomUniform(1,3),TIndex(1),TIndex(0,2), 2); 
	cout << matrix2d(TIndex(1),TIndex(0,2), 2) << endl; 
	matrix3d.Set(RandomUniform(2,2),TIndex(0,1),2,TIndex(0,2)); 
	cout << matrix3d(TIndex(0,1),2,TIndex(0,2)); 

	matrix3d.Set(RandomNormal(2),TIndex(1,2), 2,3); 
	cout << matrix3d(TIndex(1,2), 2,3) << endl; 
	matrix3d.Set(RanomNormal(5),0,2,TIndex(0,4)); 
	cout << matrix3d(0,2,TIndex(0,4)) << endl; 
	matrix3d.Set(RandomUniform(2),1,TIndex(0,2,2),4); 
	cout << matrix3d(1,TIndex(0,2,2),4); 

	cout << matrix3d(0,1,2) << endl; 
	cout << matrix3d(0,1) << endl; 

	cout << "Sum" << endl; 
	cout << matrix3d.sum(0) << endl; 
	cout << matrix3d.sum(0,1) << endl; 
	cout << matrix3d.sum() << endl; 
	matrix3d.Clear(); 
	cout << matrix3d.Size() << endl; 
}

