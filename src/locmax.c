
#include "MSExample.h"

#include <stdbool.h>

SEXP C_locmax(SEXP x, SEXP halfWindow) {
	int r = Rf_asInteger(halfWindow);
	int len = LENGTH(x);
	SEXP isLocMax;
	PROTECT(isLocMax = Rf_allocVector(LGLSXP, len));
	int * locmax = LOGICAL(isLocMax);
	for ( int i = 0; i < len; ++i )
		locmax[i] = false;
	double * y = REAL(x);
	for ( int i = r; i < len - r; ++i )
	{
		locmax[i] = true;
		for ( int j = i - r; j <= i + r; ++j )
		{
			if ( y[j] > y[i] )
			{
				locmax[i] = false;
				break;
			}
		}
	}
	UNPROTECT(1);
	return isLocMax;
}

