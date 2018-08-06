#include<stdio.h>

#define SIZE 10

void v_exchange(int a[]);
void v_exchange_ptr(int *a);

int main(void)
{
	int i, x[SIZE];  /* x[] has 10 int elements */
	for (i=0; i<SIZE; i++)
		x[i] = i;	/* assign i to x[i] */

	v_exchange(x); /* call for value exchange */

	printf("Array\n");
	for (i=0; i<SIZE; i++)
		printf("x[%d]=%d, &x[%d]=%p\n", i, x[i], i, &x[i]);

	v_exchange_ptr(&x[0]); /* call for value exchange with ptr */
	printf("\nPointer\n");
	for (i=0; i<SIZE; i++)
		printf("x[%d]=%d, &x[%d]=%p\n", i, x[i], i, &x[i]);

  return 0;
}

/* Array notation */
void v_exchange(int a[])
{
	int i;
	for (i = 0; i < SIZE/2; i++) {
		int tmp = a[i];
		a[i] = a[SIZE-1-i];
		a[SIZE-1-i] = tmp;
	}
}

/* Pointer notation */
void v_exchange_ptr(int *a) {
	/* use a different method here because copy v_exchange
	 * would be no fun and using pointers as iterators is
	 */
	int i;
	int *end = (a + SIZE - 1); /* pointer at end of array*/
	for (i = 0; i < SIZE/2; i++) {
		int tmp = *a;
		*a = *end;
		*end = tmp;

		a++; /* move the pointers closer */
		end--;
	}

}
