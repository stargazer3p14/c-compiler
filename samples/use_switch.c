#include	<stdio.h>

main()
{
  int a,b;
  char ch;

  printf("Do you want to:\n");
  printf("Add, Subtract, Multipy, or Divide\n");
  /* force user to enter valid response */
  do {
       printf("Enter first letter:  ");
       //ch =getchar();
		scanf( "%c", &ch );
       printf("\n");
      } while (ch!='A' && ch!='S' && ch!='M'         && ch!='D');
  printf("Enter first number: ");
  scanf("%d", &a);
  printf("Enter second number:  ");
  scanf("%d", &b);

  switch (ch) {
     case 'A'  : printf("%d", a+b);
                 break;
     case 'S'  : printf("%d", a-b);
                 break;
     case 'M'  : printf("%d", a*b);
                 break;
     case 'D'  : if (b!=0) printf("%d", a/b);
                 break;
  }
} 