#include<time.h>
#include<stdio.h>

unsigned int sum1(unsigned int from, unsigned int to); /* recursion */
unsigned int sum2(unsigned int from, unsigned int to); /* iteration */
unsigned int sum3(unsigned int from, unsigned int to); /* algorithm */
/* algorithm: sum(1..n) = n*(n+1)/2, sum(m..n) = sum(1..n) - sum(1..m-1) */

int main(void)
{
	unsigned int s;
	unsigned int from, to;
	double t1, t2;
	printf("Enter the fist integer:");
    scanf(" %d", &from);
    printf("Enter the second integer:");
    scanf(" %d", &to);

    t1 = clock();
    s = sum1(from, to);
    t2 = clock();
    printf("sum1=%u, %fseconds.\n", s, (t2-t1)/CLOCKS_PER_SEC);

    t1 = clock();
    s = sum2(from, to);
    t2 = clock();
    printf("sum2=%u, %fseconds.\n", s, (t2-t1)/CLOCKS_PER_SEC);

    t1 = clock();
    s = sum3(from, to);
    t2 = clock();
    printf("sum3=%u, %fseconds.\n", s, (t2-t1)/CLOCKS_PER_SEC);

	return 0;
}

unsigned int sum1(unsigned int from, unsigned int to)
{
	/* can causes segfault due to stack over flow */
	return (from == to ? from : from + sum1(from+1u, to));
}


unsigned int sum2(unsigned int from, unsigned int to)
{
	unsigned int sum = 0, i;
	for (i = from; i <= to; i++) {
		sum += i;
	}
	return sum;
}

/*
 * warning: starts to diverge from sum1 and sum2 after to*(to+1) is greater
 * than max value of unsigned int
 */
unsigned int sum3(unsigned int from, unsigned int to)
{
	/* an attempt to lower gaint number quickly to stop
	 * integer overflow from destroying the output
	 */
	to = ((to)*((2u*to+2u)/2u))/2u;
	from = ((from*(2u*from-2u))/2u)/2u;
	return to - from;
}
