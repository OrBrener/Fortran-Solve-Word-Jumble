CC = clang
CFLAGS = -std=c99 -Wall -pedantic

all: a1

clean:
	rm -f *.o a1

jumble.o: jumble.c jumble.h
	$(CC) $(CFLAGS) -c jumble.c -o jumble.o

main.o: main.c jumble.h
	$(CC) $(CFLAGS) -c main.c -o main.o

a1: main.o jumble.o
	$(CC) main.o jumble.o -o a1
