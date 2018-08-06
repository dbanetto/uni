#include<stdio.h>

int stringlen(const char *s ); /* prototype */

int main(void)
{
	char string[ 80 ]; /* create char array */

	printf("Enter a string: ");
	scanf("%[^\n]", string );
	printf("%d\n", stringlen( string ) );

	return 0;
}

int stringlen(const char *s)
{
	int len = 0;
	while (*s) { /* keep going till *s is zero, aka '\0' */
		len++;
		s++;
	}
	return len;
}
