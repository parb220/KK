#include <iostream>
#include "dw_dense_matrix.hpp"
#include "TMultiDimArray.hpp"

using namespace std; 

int main(int argc, char **argv)
{
	TDenseVector vector; 
	vector.RandomNormal(10); 
	TMultiDimArray array(10, vector); 
	cout << array(0) << endl << array << endl;  

	array.Reshape(6,2); 
	cout << array(0,false) << endl; 
	TMultiDimArray sub_array(array(TIndex(0,2,5))); 

	cout << sub_array.Dim()<<endl;  
	for (int i=0; i<sub_array.Dim(); i++)
		cout << sub_array.Size(i) << endl;

	if (sub_array.Dim() == 2)
	{
		for (int i=0; i<sub_array.Size(0); i++)
			cout << sub_array(i,false) << endl; 
	} 
}


