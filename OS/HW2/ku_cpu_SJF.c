#include <linux/syscalls.h>
#include <linux/kernel.h>
#include <linux/linkage.h>
#include <linux/sched.h>
#define IDLE 100
#define MAX 50

pid_t now = IDLE;

typedef struct jobobj
{
	pid_t pid;
	int job;
} job_t;

typedef struct queue
{
	int first;
	int last;
	job_t jobs[MAX];
} queue_t;

queue_t wq;

job_t ku_pop2(void);
void ku_push2(job_t new);
bool ku_is_empty2(void);
bool ku_is_new2(pid_t chkpid);
void ku_compinsert2(job_t new);

SYSCALL_DEFINE2(oslab_ku_cpu2_sjf, char*, name, int, job) {
	job_t new_job = {current->pid, job};
	if (now == IDLE) now = new_job.pid;
	if (now == new_job.pid) {
		if (job == 0) {
			printk("Process Finish: %s\n", name);
			if (ku_is_empty2())
				now = IDLE;
			else
				now = (ku_pop2()).pid;
		} 
		else {
			printk("Working: %s\n", name);
		}
		return 0;
	}
	else {
		if (ku_is_new2(new_job.pid)) ku_compinsert2(new_job);
		printk("Working Denied: %s\n", name);
	}
	return 1;
}

job_t ku_pop2(void)
{
	wq.first = (wq.first + 1) % MAX;
	return wq.jobs[wq.first];
}
void ku_push2(job_t new)
{
	wq.last = (wq.last + 1) % MAX;
	wq.jobs[wq.last] = new;
}
bool ku_is_empty2(void)
{
	if (wq.first == wq.last) return true;
	return false;
}
bool ku_is_new2(pid_t chkpid)
{
	int i = 0;	
	for (i = (wq.first + 1) % MAX; i != (wq.last + 1) % MAX; i = (i + 1) % MAX) {
		if ((wq.jobs[i]).pid == chkpid) return false;
	}
	return true;
}
void ku_compinsert2(job_t new)
{
    int i = 0;
    int insertsuc = 0;
	int scanfirst = wq.first + 1;
	int scanlast = wq.last + 1;
    for (i = (scanfirst % MAX); i != (scanlast % MAX); i = (i + 1) % MAX) {
        if ((wq.jobs[i]).job <= new.job || insertsuc == 1) {
            ku_push2(ku_pop2());
        }
        else {
            ku_push2(new);
            ku_push2(ku_pop2());
            insertsuc = 1;
			i = (i + 1) % MAX;
        }
    }
    if (insertsuc == 0) ku_push2(new); //순회하면서 순서에 job 크기에 맞게 정렬
}