#include <stdio.h>

main()
{
    int balance;
    int *address;
    int value;

    balance = 5000;

    address = &balance;
    value   = *address;

    printf("Balance is : %d\n" , value);
}

