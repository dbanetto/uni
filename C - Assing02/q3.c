#include<stdio.h>
int stringcmp(const char *s1, const char *s2 ); /* prototype */

int main(void)
{
	char string1[ 80 ]; /* create a string */
	char string2[ 80 ]; /* create another string */
	printf("Enter two strings: ");

	scanf("%s%s", string1 , string2);

	printf("The result is %d\n", stringcmp(string1, string2));
	return 0;
}

int stringcmp(const char *s1, const char *s2) {

	while (*s1 && *s2) { /* keep going till the end of a string */
		if (*s1 != *s2) {
			return 1;
		}
		s1++;
		s2++;

	}
	if (*s1 != *s2) { /* make sure both are at end of strings */
		return 1;
	}
	return 0;
}
