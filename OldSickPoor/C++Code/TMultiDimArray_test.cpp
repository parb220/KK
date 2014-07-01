#include <iostream>
#include "dw_dense_matrix.hpp"
#include "TMultiDimArray.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	TDenseVector vector; 
	vector.RandomNormal(10); 
	TMultiDimArray array(10, vector); 
	cout << array(0, SCALAR) << endl << array << endl;  

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
	TMultiDimArray sub_array_7(array(TIndex(0,2), TIndex(0), TIndex(0), TIndex(0,1))); 	
	cout << sub_array_7.Dim() << endl; 
	cout << sub_array_7(0,0,0) << endl; 
}


