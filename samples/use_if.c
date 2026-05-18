#include <stdio.h>

int	a = 3, b = 84;

main()
{

	printf( "input a, b: " );
	scanf( "%d %d", &a, &b );

	if ( b )
		printf( "a / b = %d\n", a / b );
	else
		printf( "can't divide by zero!\n" );

	return	1;
}

