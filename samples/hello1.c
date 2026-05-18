#include <stdio.h>

#define	MYDEF	5

#ifdef	MYDEF
char	*a;
 #if 1
	long	b;
 #endif
#else
int	bbb;
#endif

main()
{
#ifdef	MYDEF
	b = MYDEF;
#else
	bbb = 27;
#endif

	printf( "Hello, world!\n" );
	return	1;
}

