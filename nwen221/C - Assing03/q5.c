#include <stdio.h>

struct date {
	int month, day, year;
};

int compare_dates(struct date, struct date);
void put_date(struct date);

int main(void) {
	struct date d1, d2;

	printf("Enter first date (mm/dd/yy)\n");
	scanf(" %i/%i/%i", &d1.month, &d1.day, &d1.year);

	printf("Enter first date (mm/dd/yy)\n");
	scanf(" %i/%i/%i", &d2.month, &d2.day, &d2.year);

	if (compare_dates(d1, d2) < 0) {
		put_date(d1);
		printf(" is earlier than ");
		put_date(d2);
	} else {
		put_date(d2);
		printf(" is earlier than ");
		put_date(d1);
	}
	printf("\n");

	return 0;
}

int compare_dates(struct date d1, struct date d2) {
	if (d1.year != d2.year) {
		return d1.year - d2.year;
	} else if (d1.month != d2.month) {
		return d1.month - d2.month;
	} else if (d1.day != d2.day) {
		return d1.day - d2.day;
	}

	return 0;
}

void put_date(struct date d) {
	printf("%i/%i/%i", d.month, d.day, d.year);
}
