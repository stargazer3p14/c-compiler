#include <stdio.h>

int	main()
{
	int	a, b, c;

    printf( "Input two numbers... \n" );
    scanf( "%d %d", &a, &b );
    c  =  a * b ;
    printf( "%d times %d = %d\n", a, b, c );
	return	0;
}

