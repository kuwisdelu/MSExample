
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector rcpp_locmax(NumericVector x, int halfWindow) {
	LogicalVector isLocMax = LogicalVector(x.length());	
	for ( int i = halfWindow; i < x.length() - halfWindow; ++i )
	{
		isLocMax[i] = true;
		for ( int j = i - halfWindow; j <= i + halfWindow; ++j )
		{
			if ( x[j] > x[i] )
			{
				isLocMax[i] = false;
				break;
			}
		}
	}
	return isLocMax;
}
