#include <stdio.h>

main()
{
    char	*c , ch[10];
    int		*i , j[10];
    double	*d , g[10];
    int		x;

    c = &ch[0];
    i = &j[0];
	d = &g[0];

    for ( x=0 ; x<10 ; x++ )
		printf("%p %p %p \n" , i+x, c+x, d+x );
}


