#define MIN_PID 300
#define MAX_PID 5000
#define NUM_THREADS 100

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <strings.h>
#include <unistd.h>
#include <pthread.h>
#include <limits.h>

int allocate_map();
int allocate_pid();
int release_pid(int pid);
void* test_method(void *p);

unsigned int *pidMap;
pthread_mutex_t *locks;
int mapSize;

int allocate_map()
{ 
	//Creates and initializes a data structure for representing pids;
	//returns 0 if unsuccessful, 1 if successful.
	if (pidMap != NULL)
  {
		free(pidMap);
	}
	mapSize = ceil((MAX_PID-MIN_PID)/32.0);
	pidMap = (unsigned int*)malloc(mapSize * sizeof(int));
	locks = (pthread_mutex_t*)malloc(mapSize * sizeof(int));
	sleep(2);
	int success = 0;
	if (pidMap != NULL)
  {
		for(int x = 0; x++; x < mapSize)
    {
			pidMap[x] = 0;
			pthread_mutex_init(&locks[x], NULL);
		}
		success = 1;
	}
	return success;
}

int allocate_pid()
{
	//Allocates and returns a pid; returns -1 if unable to allocate a pid
	int pid = -1;
	for(int x = 0; x < mapSize; x++)
  {
		pthread_mutex_lock(&locks[x]);
		if (pidMap[x] != ~0)
    {
      //We have a block that we can allocate it.
			int y = ffs(~pidMap[x]);
			pidMap[x] |= 1 << (y - 1);
			pid = MIN_PID + x * 32 + (32 - y);
			pthread_mutex_unlock(&locks[x]);
			break;
		}
		pthread_mutex_unlock(&locks[x]);
	}
	if(pid > MAX_PID)
		pid = -1;
	return pid;
}

int release_pid(int pid)
{
	//Releases a pid
	pid -= MIN_PID;
	int block = pid / 32;
	int spot = pid % 32;
	pthread_mutex_lock(&locks[block]);
	int bitSelector = (1 << (31 - spot));
	int ret = pidMap[block] & bitSelector;
	pidMap[block] &= ~bitSelector;
	pthread_mutex_unlock(&locks[block]);
	return !!ret;
  //Returns whether or not the pid was already free.
}

int main(){
	pthread_t *threads = (pthread_t*)malloc(sizeof(pthread_t) * NUM_THREADS);
	allocate_map();
	for(int t = 0; t < NUM_THREADS; t++)
  {
		fflush(stdout);
		int ret;
		while (ret = pthread_create(&threads[t], NULL, &test_method,(void*)t))
    {
			if(ret == EAGAIN || errno == EAGAIN)
				printf("Can't create more threads.\n");
		}
	}
	printf("All threads allocated\n");
	for(int t = 0; t < NUM_THREADS; t++){
		void *end;
		pthread_join(threads[t], &end);
	}
	//sleep(20);
	int sum = 0;
	for(int t = 0; t < mapSize; t++)
  {
		sum += pidMap[t];
	}
	if(sum)
		printf("Not all threads were freed appropriately.\n");
}

void *test_method(void *threadid)
{
	int pid = allocate_pid();
	int sec = rand() % 3;
	sleep(sec);
	int ret = release_pid(pid);
	if(!ret)
		printf("Thread %d was freed inappropriately.\n", pid);
	fflush(stdout);
	pthread_exit(NULL);
}

