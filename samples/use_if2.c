#include <stdio.h>

int a,b;
int	ch;

main()
{
  printf("Do you want to: \n");
  printf("Add, subtract, Multiply, or Divide?\n");

  /* force user to enter valid response */
  do {
       printf("Enter first letter:  ");
       //ch=getchar();
	   scanf( "%c", &ch );

       printf("\n");
     } while ( ch != 'A' && ch!='S' && ch!='M'         && ch!='D');
     printf("Enter first number: ");
     scanf("%d", &a);
     printf("Enter second number:  ");
     scanf("%d", &b);

  if (ch=='A') printf("%d", a+b);
  else if (ch=='S') printf("%d", a-b);
  else if (ch=='M') printf("%d", a*b);
  else if (ch=='D' && b!=0) printf("%d", a/b);
}