#include <stdio.h>

/* Name: David Barnett   */
/* Student ID: 300313764 */

int main() {

	int col, row;
	for (row = 1; row <= 4; row++) {
		for (col = 1; col <= row; col++) {
			printf("*");
		}
		printf("\n");
	}

	return 0;
}
