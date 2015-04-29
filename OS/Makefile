all: pidManager copy
	@true

pidManager: pidManager.c
	gcc -o pidManager pidManager.c -std=c11 -lpthread

copy: copy.c
	gcc -o copy copy.c

clean:
	rm -f pidManager copy 
