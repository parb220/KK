#ifndef _INTERPOLATE_HEADER
#define _INTERPOLATE_HEADER

class TDenseVector; 
class TIndex; 

int Locate(const TDenseVector &xvect, double x, int &loc); 
int Locate(const TIndex &xvect, int x, int &loc); 

int get_interps(const TDenseVector &xvect, double x, int &loc, double &mx); 

int Interpolate(const TDenseVector &xvect, const TDenseVector &yvect, const TDenseVector &x1vect, TDenseVector &y1vect); 
int Interpolate(const TDenseVector &xvect, const TDenseVector &yvect, double x1, double &y1); 
#endif
