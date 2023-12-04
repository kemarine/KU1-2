#include <linux/syscalls.h>
#include <linux/kernel.h>
#include <linux/linkage.h>
#include <linux/sched.h>
#define IDLE 100 // default value of "now"
#define MAX 50 // max size of queue - especially waiting queue

pid_t now = IDLE; // process pid currently using CPU
int now_job = -1; // remaining job of process current used CPU - for SRTF
int timeslice = 10; // time slice(length) - for RR

typedef struct jobobj // job type
{
	pid_t pid;
	int job;
} job_t;

typedef struct queue // queue type, used circle queue to avoid overflow
{
	int first;
	int last;
	job_t jobs[MAX];
} queue_t;

queue_t wq; // waiting queue

job_t ku_pop2(void); // delete and return first object of wq
void ku_push2(job_t new); // add new object in wq
bool ku_is_empty2(void); // check whether wq is empty
bool ku_is_new2(pid_t chkpid); // check whether process already exists in wq
void ku_compinsert2(job_t new); // add new object in wq and sort by size of "job"

SYSCALL_DEFINE2(oslab_ku_cpu2_fcfs, char*, name, int, job) { // FCFS
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
		if (ku_is_new2(new_job.pid)) ku_push2(new_job);
		printk("Working Denied: %s\n", name);
	}
	return 1;
}

SYSCALL_DEFINE2(oslab_ku_cpu2_sjf, char*, name, int, job) { // SJF
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
		if (ku_is_new2(new_job.pid)) ku_compinsert2(new_job); // after inserting new process, sort waiting queue
		printk("Working Denied: %s\n", name);
	}
	return 1;
}

SYSCALL_DEFINE2(oslab_ku_cpu2_strf, char*, name, int, job) { // SRTF (typo)
	job_t new_job = {current->pid, job};
	if (now == IDLE) {
		now = new_job.pid;
	}
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
		now_job = job - 1; // request successfully accepted -> remaining job size decreases
		return 0;
	}
	else {
		if (job < now_job) { // if currently requesting process's remaining job size is smaller -> change process that owns cpu
			now = new_job.pid;
			printk("Working: %s\n", name);
			now_job = job - 1;
            return 0;
		}
		else if (ku_is_new2(new_job.pid)) { // else -> same with sjf
            ku_compinsert2(new_job);
		    printk("Working Denied: %s\n", name);
        }
	    return 1;
    }
}

SYSCALL_DEFINE2(oslab_ku_cpu2_rr, char*, name, int, job) { // RR
	job_t new_job = {current->pid, job};
	if (now == IDLE) {
		now = new_job.pid;
	}
	if (now == new_job.pid) {
		if (job == 0) {
			printk("Process Finish: %s\n", name);
			timeslice = 10; // job finishes -> reset time slice
			if (ku_is_empty2())
				now = IDLE;
			else
				now = (ku_pop2()).pid;
			return 0;
		}
		if (timeslice == 0) { // if requesting process's remaining time slice is zero -> change CPU to idle state
			printk("----> Turn Over: %s\n", name);
			ku_push2(new_job);
			if (ku_is_empty2()) now = IDLE;
			else now = (ku_pop2()).pid;
			timeslice = 10; // reset time slice 
			return 1;
		}
		printk("Working: %s\n", name);
		timeslice--; // request successfully accepted -> 0.1 sec decreases from remaining time slice
		return 0;
	}
	else {
		if (ku_is_new2(new_job.pid)) ku_push2(new_job);
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
	job_t tmp;

	ku_push2(new); // push new job
	
	for (i = (wq.first + 1) % MAX; i != (wq.last) % MAX; i = (i + 1) % MAX) { // sort by size of job
		if ((wq.jobs[i]).job > (wq.jobs[(i + 1) % MAX]).job) {
			tmp = wq.jobs[i];
			wq.jobs[i] = wq.jobs[(i + 1) % MAX];
			wq.jobs[(i + 1) % MAX] = tmp;
		}
	}
}