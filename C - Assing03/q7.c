#include <stdio.h>

void swap_ptr(int **p, int **q) {
	int *tmp = *p;
	*p = *q;
	*q = tmp;
}

int main(void) {
	int p = 11, q= 22;
	int *ptrp = &p;
	int *ptrq = &q;
	int **ppp = &ptrp;
	int **ppq = &ptrq;

	printf("p:%p q:%p\n", ptrp, ptrq);
	swap_ptr(ppp, ppq);
	printf("p:%p q:%p\n", ptrp, ptrq);

	return 0;
}
