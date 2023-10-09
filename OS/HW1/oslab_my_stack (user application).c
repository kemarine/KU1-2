#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main (){
	int r = 0, cnt = 0, rndint = 0, i;
	srand(time(NULL));

	while (cnt < 3) {
		rndint = (rand() % 100) + 1;
		r = syscall ( 335, rndint );
		printf("push : %d\n", rndint);
		if (r == -1) continue;
		else cnt++;
	}	
		
	
	for (i = 0; i < 4; i++) {
		r = syscall(336);
		printf("pop : %d\n", r);
	}	
	
	return 0;
}
