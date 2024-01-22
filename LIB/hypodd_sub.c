
/*  all c-subroutines needed for hypodd. when put together there was a conflict between
    atoangle_ and atoangle so atangle was renemed atanglexx.
    all static char rcsid commented out since they refer to a particular computer.
    F77SLA were replaced by int to make it more generral and f77types.h was then
    not included */


#ifndef lint
//static char rcsid[]="$Header: /home1/crhet/julian/HYPODD/hypoDD/RCS/atoangle.c,v 1.1 2001/02/15 02:21:44 julian Exp $";//
#endif lint
#include <string.h>
#include <stdlib.h>

#ifndef NULL
#define NULL	0
#endif NULL

/*
 * Convert string of form "degrees[:minutes[:seconds]]" to angle
 * Also useful for times
 */
double
atoanglexx(p)
const	char	*p;
{
	double	sign;
	double	d, m, s;

	sign = 1.0;
	if (*p == '+')
		p++;
	if (*p == '-') {
		sign = -1.0;
		p++;
	}
	d = atof(p);
	m = s = 0.0;
	if ((p = strchr(p, ':')) != NULL) {
		m = atof(++p);
		if ((p = strchr(p, ':')) != NULL)
			s = atof(++p);
	}
	return sign*(d + (m + s/60.0)/60.0);
}



#ifndef lint
//static char rcsid[]="$Header: /home1/crhet/julian/HYPODD/hypoDD/RCS/atoangle_.c,v 1.1 2001/02/15 02:12:22 julian Exp $";//
#endif lint
#include <string.h>
#include "compat.h"
//#include "f77types.h"//

	double	atoangle PARMS((char*));

/*
 * f77-callable interface to atoangle function
 */
double
atoangle_(p, l)
const	char	*p;	/* Character string		(input)	*/
	int	l;	/* Length of *p			(input)	*/
{
	char	q[l+1];	/* Buffer, with space for terminator	*/

	return atoanglexx(strncpy(q, p, l));
}



#ifndef lint
//static char rcsid[] = "$Header: /home1/crhet/julian/HYPODD/hypoDD/RCS/datetime_.c,v 1.1 2001/02/15 21:34:55 julian Exp $";//
#endif /* lint */

#include <string.h>
#include <time.h>
//#include "f77types.h"//

#define MIN(a, b) ((a) < (b) ? (a) : (b))

/*
 * Give current date & time as a character string
 * Full string requires 25 bytes, including null terminator.  Else truncated.
 * f77-callable
 */
void
datetime_(s, n)
	char	*s;
	int	n;
{
	time_t	now;

	now = time(NULL);
	strncpy(s, ctime(&now), MIN(n-1, 24));
}
#ifndef lint
//static char rcsid[] = "$Header: /home1/crhet/julian/HYPODD/hypoDD/RCS/hypot_.c,v 1.1 2001/02/14 00:21:19 julian Exp $";//
#endif /* lint */

#include <math.h>

/* f77-callable interface to hypot function */
double
hypot_(a, b)
	float	*a, *b;
{
	return hypot((double)*a, (double)*b);
}
#ifndef lint
//static char rcsid[] = "$Header: /home1/crhet/julian/HYPODD/hypoDD/RCS/rpad_.c,v 1.1 2001/02/15 20:02:38 julian Exp $";//
#endif /* lint */

#include <ctype.h>
//#include "f77types.h"//

/*
 * Pad character string to full length with white space (remove null terminator)
 * f77-callable
 */
void
rpad_(s, n)
	char	*s;
        int n;
//	F77SLA	n;//
{
	int	i, j;

	for (i=0; i<n; i++) {
		if (s[i] == '\0') {
			/* Found terminator: Put spaces from here to end */
			for (j=i; j<n; j++)
				s[j] = ' ';
			break;
		}
	}
}
#ifndef lint
//static char rcsid[]="$Header: /home1/crhet/julian/HYPODD/hypoDD/RCS/sscanf3_.c,v 1.1 2001/02/15 02:12:51 julian Exp $";//
#endif /* lint */

#include <stdio.h>
#include "compat.h"
//#include "f77types.h"//

/*
 * f77-callable interface to sscanf() function
 * Klewdgy version that allows three pointer arguments
 */
//F77INT//
sscanf3_(s, fmt, p1, p2, p3)
const	char	*s;
const	char	*fmt;
	GENPTR	p1, p2, p3;
{

	return sscanf(s, fmt, p1, p2, p3);
}
