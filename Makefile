CC = gcc
CFLAGS = -Wall

FLORI_LIBS =

build: flori ;

bin:
	mkdir bin

%.o: %.c
	$(CC) $(CFLAGS) -c $<

flori: bin $(FLORI_LIBS) flori.o
	$(CC) $(CFLAGS) -o bin/flori $(FLORI_LIBS) flori.o

clean:
	rm -rf bin/ *.out *.asm *.o

