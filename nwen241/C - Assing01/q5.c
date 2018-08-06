#include <stdio.h>

/* Name: David Barnett   */
/* Student ID: 300313764 */

int main () {
	int aCount = 0; /* total a grades */
	int bCount = 0; /* total b grades */
	int cCount = 0; /* total c grades */
	int dCount = 0; /* total d grades */
	int eCount = 0; /* total e grades */

	int loop = 1;
	while (loop) {
		char grade = '\0';
		printf("Enter grade: ");
		scanf(" %c", &grade);
		switch (grade) {
			case 'A':
			case 'a':
				aCount++;
				break;
			case 'B':
			case 'b':
				bCount++;
				break;
			case 'C':
			case 'c':
				cCount++;
				break;
			case 'D':
			case 'd':
				dCount++;
				break;
			case 'E':
			case 'e':
				eCount++;
				break;

			case 'Q':
			case 'q':
				loop = 0; /* stop loop */
				break;

			default:
				printf("Invalid grade \'%c\', use Q to quit\n", grade);
		}

	}

	printf("A grades count: %d\n", aCount);
	printf("B grades count: %d\n", bCount);
	printf("C grades count: %d\n", cCount);
	printf("D grades count: %d\n", dCount);
	printf("E grades count: %d\n", eCount);

	return 0;
}
