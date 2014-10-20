#include <iostream>
#include <string>
#include "dw_dense_matrix.hpp"
#include "TMultiDimArray.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	int dim = 7; 

	// TDenseVector object used to form a dim-dimensional TMultiDimArray object
	TDenseVector vector(2 << (dim-1)); 
	for (int i=0; i<vector.dim; i++)
		vector[i] = i;  

	// Construct TMultiDimArray object
	std::vector<int> n(dim,2); 
	TMultiDimArray array(n, vector); 

	// Dimension and sizes along dimensions
	cout << "Dimension: " << array.Dim() << endl; 
	cout << "Sizes along dimensions: " << endl; 
	for (int d=0; d<array.Dim(); d++)
		cout << "Dim " << d << ": " << array.Size(d) << endl; 

	// Reshape to (dim-1)-dimensional TMultiDimArray object
	if (array.Dim() > 1)
	{
		std::vector<int> new_n(array.Dim()-1); 
		for (int d=0; d<array.Dim()-1; d++)
			new_n[d] = array.Size(d); 
		new_n[new_n.size()-1] = array.Size(array.Dim()-2)*array.Size(array.Dim()-1); 
		array.Reshape(new_n); 
		cout << "After reshaping, dimension: " << array.Dim() << endl; 
		cout << "After reshaping, sizes along dimensions: " << endl; 
		for (int d=0; d<array.Dim(); d++)
			cout << "Dim " << d << ": " << array.Size(d) << endl; 
	}
		
	// Reshape back 
	array.Reshape(n); 
		
	// Get and set values
	cout << "Element (0,0,0,0,0,0,1): " << array(0,0,0,0,0,0,1,SCALAR) << endl; 
	cout << "Corresponding to the 64-th element: " << vector[64] << endl; 
	array.Set(array(0,0,0,0,0,0,1,SCALAR)*2.0, 0,0,0,0,0,0,1); // doubled the original value
	cout << "Element (0,0,0,0,0,0,1) being doubled: " << array(0,0,0,0,0,0,1,SCALAR) << endl; 
	 array.Set(array(0,0,0,0,0,0,1,SCALAR)/2.0, 0,0,0,0,0,0,1); // recover

	// TIndex objects used to specify indices
	TMultiDimArray sub_array_1=array(0,TIndex(0,1),0,0,0,TIndex(0,1),1); 
	TMultiDimArray sub_array_2=array(1,TIndex(0,1),1,1,1,TIndex(0,1),1); 

	// + operation	
	TMultiDimArray sub_array_sum = sub_array_1 + sub_array_2; 
	cout << "dimension: " << sub_array_sum.Dim() << endl; 
	cout << "(0,0) : " << sub_array_1(0,0) << "\t" << sub_array_2(0,0) << "\t" << sub_array_sum(0,0) << endl; 
	cout << "(0,1) : " << sub_array_1(0,1) << "\t" << sub_array_2(0,1) << "\t" << sub_array_sum(0,1) << endl;
	cout << "(1,0) : " << sub_array_1(1,0) << "\t" << sub_array_2(1,0) << "\t" << sub_array_sum(1,0) << endl;
	cout << "(1,1) : " << sub_array_1(1,1) << "\t" << sub_array_2(1,1) << "\t" << sub_array_sum(1,1) << endl;
	cout << "For reference " << array(0,0,0,0,0,0,1) << "\t" << array(1,0,1,1,1,0,1) << endl;  
	cout << "For reference " << array(0,0,0,0,0,1,1) << "\t" << array(1,0,1,1,1,1,1) << endl;	
	cout << "For reference " << array(0,1,0,0,0,0,1) << "\t" << array(1,1,1,1,1,0,1) << endl;
	cout << "For reference " << array(0,1,0,0,0,1,1) << "\t" << array(1,1,1,1,1,1,1) << endl;

	// sum
	cout << "Sum: " << array.sum() << endl; // should be 8128 = 0 + 1 + 2 + 3 + ... + 127 
	// along 0-th 
	TMultiDimArray sum_array = array.sum(0); // should be a 2x2x2x2x2x2 array 
	cout << "Sum: " << sum_array(0,0,0,0,0,0,SCALAR) << "\t" << sum_array(1,0,0,0,0,0,SCALAR) << endl;  

	// along 0-th, 1-th, 2-th and 3-th
	sum_array = array.sum(0,1); // should be a 2x2x2x2x2 array
	cout << "Sum: " << sum_array(0,0,0,0,0,SCALAR) << "\t" << sum_array(1,0,0,0,0,SCALAR) << "\t" << sum_array(0,1,0,0,0,SCALAR) << "\t" << sum_array(1,1,0,0,0,SCALAR) << endl; 
}


	/*cout << array(0, SCALAR) << endl << array << endl;  

	array.Reshape(6,2); 
	cout << array(2,1,SCALAR) << endl; 
	cout << array(0) << endl; 
	TMultiDimArray sub_array(array(TIndex(0,2,5))); 

	cout << sub_array.Dim()<<endl;  
	for (int i=0; i<sub_array.Dim(); i++)
		cout << sub_array.Size(i) << endl;

	if (sub_array.Dim() == 2)
	{
		for (int i=0; i<sub_array.Size(0); i++)
			cout << sub_array(i) << endl; 
	}

	array.Reshape(2,2,3); 
	TMultiDimArray sub_array_2(array(1));  
	cout << sub_array_2.Dim() << endl; 
	cout << sub_array_2(0) << endl; 
	cout << sub_array_2(1) << endl; 

	cout << array(1,1) << endl; 
	TMultiDimArray sub_array_3(array(1,TIndex(0,1))); 
	cout << sub_array_3.Dim() << endl; 
	cout << sub_array_3(0) << endl; 
	cout << sub_array_3(1) << endl; 

	array.Set(TMultiDimArray(4,vector.RandomUniform(4)),TIndex(0,1),TIndex(0,1),2); 
	TMultiDimArray sub_array_4(array(TIndex(0,1), TIndex(0,1), 2)); 
	cout << sub_array_4.Dim() << endl; 
	cout << sub_array_4(0) << endl; 
	cout << sub_array_4(1) << endl; 

	array.Set(TMultiDimArray(2,vector.RandomNormal(2)), 0, TIndex(0,1), 2); 
	TMultiDimArray sub_array_5(array(0,TIndex(0,1),2)); 
	cout << sub_array_5 << endl; 

	array.Set(TMultiDimArray(2,vector.RandomNormal(2)), TIndex(0,1), 1, 2);
	TMultiDimArray sub_array_51(array(TIndex(0,1),1,2));
	cout << sub_array_51 << endl; 
	
	TMultiDimArray sub_array_6(array(0,0,1)); 
	cout << sub_array_6 << endl; 

	array.Set(23,0,1,2); 
	cout << array(0,1,2,SCALAR); 
	array.Reshape(3,2,2,2); 
	array.Set(TMultiDimArray(12,vector.RandomUniform(12)), TIndex(0,2), TIndex(0,1), TIndex(0,1), TIndex(1)); 
	TMultiDimArray sub_array_7(array(TIndex(0,1), 1, TIndex(0,1), 1)); 	
	cout << sub_array_7.Dim() << endl; 
	cout << sub_array_7(0) << endl; 

	array.Set(15, 2,0,1,0); 
	cout << array(2,0,1,0) << endl; 

	array.Reshape(2,2,3,2,2); 
	TMultiDimArray sub_array_8(array(0, 1, TIndex(0,2), 1, 0)); 
	cout << sub_array_8.Dim() << endl; 

	TMultiDimArray sum_1(array.sum(0)); 
	TMultiDimArray sum_2(array.sum(2)); 
	TMultiDimArray sum_3(array.sum(1,3));
	TMultiDimArray sum_4(array.sum(0,1,2,3,4)); 

	cout << array.sum() << endl; */


