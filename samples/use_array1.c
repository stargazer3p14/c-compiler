#include <stdio.h>

main()
{

	int i[10];
	int	j;

    for ( j=0 ; j<10 ; j++) 
      i[j] = j ;

    for ( j=0 ; j<10 ; j++) 
      printf("%d\n" , i[j]) ;
}

