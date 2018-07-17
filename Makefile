CC = gcc
CFLAGS = -Wall

FLORI_LIBS = vector.o

build: flori ;

bin:
	@mkdir bin
tmp:
	@mkdir tmp
build-adhocc:
	@adhocc build adhoccfile.c

flori.h: tmp build-adhocc
	cat flori.h | adhocc > tmp/flori.h
%.o: %.c tmp flori.h
	@cp $< tmp/$<
	$(CC) $(CFLAGS) -c tmp/$<

flori: bin $(FLORI_LIBS) flori.o
	$(CC) $(CFLAGS) -o bin/flori $(FLORI_LIBS) flori.o

test: flori
	./test.sh

clean:
	@rm -rf bin/ *.out *.asm *.o adhocctmp/ tmp/

