#include <linux/syscalls.h>
#include <linux/kernel.h>
#include <linux/linkage.h>

int stack[100];
int top = -1;

SYSCALL_DEFINE1(os2023_push, int, a){
	printk(KERN_INFO "[System call] os2023_push() :\n");
	int chk = 0;
	int i;	
	if (top < 99){
		for (i = 0; i <= top; i++){
			if (stack[i] == a){
				chk = 1;	
			} 
		}	
		if (!chk) {
			top++;
			stack[top] = a;
		}
	}
	else {
		printk(KERN_NOTICE "Error : Stack is Full\n");
	}
	printk("Stack top ----------------\n");
	for (i = top; i >= 0; i--){
		printk("%d\n", stack[i]);
	}
	printk("Stack bottom -------------\n");
	
	if (chk == 1) return -1;
	return 0;
}

SYSCALL_DEFINE0(os2023_pop){
	printk(KERN_INFO "[System call] os2023_pop() :\n");
	int i;
	int j;
	int tmp;	
	if (top > -1){
		for (i = 0; i < top; i++){
			for (j = 0; j < top - i; j++){
				if (stack[j] > stack[j + 1]){
					tmp = stack[j];
					stack[j] = stack[j + 1];
					stack[j + 1] = tmp;
				}
			}
		}
		printk("Stack top ----------------\n");
		for (i = top - 1; i >= 0; i--){
			printk("%d\n", stack[i]);
		}
		printk("Stack bottom -------------\n");

		return stack[top--];
	}
	printk(KERN_NOTICE "Error : Stack is Empty");
	printk("Stack top ----------------\n");
	printk("Stack bottom -------------\n");
	
	return -1;
}
