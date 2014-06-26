#include <iostream>
#include "TDenseMatrix3D.hpp"
#include "dw_dense_matrix.hpp"
#include "TDenseMatrix4D.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	/*TDenseMatrix3D matrix3d; 
	TDenseMatrix matrix2d; 
	TDenseVector vector, result;
	matrix3d.Resize(3,4,5); 
	for (int i=0; i<matrix3d.Size(); i++)
	{
		matrix3d.Set(matrix2d.RandomUniform(4,5),i);
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

	matrix2d.RandomUniform(2,3);
	cout << matrix2d << endl;  
	matrix3d.Set(matrix2d,1,TIndex(0,1),TIndex(2,4)); 
	cout << matrix3d(1,TIndex(0,1),TIndex(2,4)) << endl; 
	matrix2d.RandomUniform(1,3); 
	cout << matrix2d << endl; 
	matrix3d.Set(matrix2d,TIndex(1),TIndex(0,2), 2); 
	cout << matrix3d(TIndex(1),TIndex(0,2), 2) << endl; 
	matrix2d.RandomUniform(2,2);
	cout << matrix2d << endl; 
	matrix3d.Set(matrix2d,TIndex(0,1),2,TIndex(0,2,3)); 
	cout << matrix3d(TIndex(0,1),2,TIndex(0,2,3)); 

	vector.RandomNormal(2); 
	cout << vector << endl; 
	matrix3d.Set(vector,TIndex(1,2), 2,3); 
	cout << matrix3d(TIndex(1,2), 2,3) << endl;
	vector.RandomNormal(5); 
	cout << vector << endl;  
	matrix3d.Set(vector,0,2,TIndex(0,4)); 
	cout << matrix3d(0,2,TIndex(0,4)) << endl; 
	vector.RandomUniform(2); 
	cout << vector << endl; 
	matrix3d.Set(vector,1,TIndex(0,2,2),4); 
	cout << matrix3d(1,TIndex(0,2,2),4); 

	cout << matrix3d(0,1,2) << endl; 
	cout << matrix3d(0,1) << endl;

	cout << "Sum" << endl; 
	cout << matrix3d.sum(0) << endl; 
	cout << matrix3d.sum(1) << endl; 
	cout << matrix3d.sum(2) << endl; 
	cout << matrix3d.sum(0,1) << endl; 
	cout << matrix3d.sum(0,2) << endl; 
	cout << matrix3d.sum(1,2) << endl; 
	cout << matrix3d.sum() << endl; 
	matrix3d.Clear(); 
	cout << matrix3d.Size() << endl; */


	TDenseMatrix4D matrix4d(2,3,4,5); 
	TDenseMatrix matrix2d; 
	for (int i=0; i<matrix4d.Size(0); i++)
		for (int j=0; j<matrix4d.Size(1); j++)
			matrix4d.Set(matrix2d.RandomUniform(matrix4d.Size(2),matrix4d.Size(3)), i,j); 

	matrix4d.Set(matrix2d.RandomNormal(matrix4d.Size(0),matrix4d.Size(3)), TIndex(0,matrix4d.Size(0)-1),2,3,TIndex(0,matrix4d.Size(3)-1)); 

	for (int k=0; k<matrix4d.Size(1); k++)
		for (int l=0; l<matrix4d.Size(2); l++)
			cout << matrix4d(TIndex(0,matrix4d.Size(0)-1),k,l,TIndex(0,matrix4d.Size(3)-1)) << endl;  

	cout << "Sum" << endl; 
	cout << matrix4d.sum(0,1) << endl; 
	cout << matrix4d.sum(0,1,2) << endl; 
	cout << matrix4d.sum() << endl;
	matrix4d.Clear(); 
	cout << matrix4d.Size() << endl; 
}

