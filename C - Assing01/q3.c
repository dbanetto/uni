#include <stdio.h>

/* Name: David Barnett   */
/* Student ID: 300313764 */

int main () {

	/* get int max value via exploiting integer overflow */
	int smallest = (1 << (sizeof(int) * 8) - 1) - 1;

	int number = 0;
	printf("Enter how many integers to be entered:");
	scanf(" %d", &number);

	int i;
	for (i = 0; i < number; i++) {
		int value = 0;
		printf("\nEnter integer:");
		scanf(" %d", &value);

		if (value < smallest) {
			smallest = value;
		}
	}

	printf("The smallest entered was: %d", smallest);
	return 0;
}
