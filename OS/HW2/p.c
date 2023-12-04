#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv) {
	int job;
	int delay;
	char name[4];
	int wait = 0;
	
	if(argc < 4) {
		printf("\nInsufficient Arguments\n");
		return 1;
	}
	
	job = atoi(argv[1]);
	delay = atoi(argv[2]);
	strcpy(name, argv[3]);

	sleep(delay);
	printf("\nProcess %s : I will use CPU by %ds.\n", name, job);
	job *= 10;

	while(job) {
		if (!syscall(335, name, job)) job--; // 335 : FCFS
		else {
			wait++;
		}
		usleep(100000);
	}

	syscall(335, name, 0);
	printf("\nProcess %s : Finish! My total wait time is %ds.\n", name, (wait + 5)/10);
	return 0;
}
