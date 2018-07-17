CC = gcc
CFLAGS = -Wall

FLORI_LIBS = string.o vector.o lexer.o tostring.o

build: flori ;

bin:
	@mkdir bin
tmp:
	@mkdir tmp
build-adhocc:
	@adhocc build adhoccfile.c

flori.h: tmp build-adhocc
	rm tostring.c
	cat flori.h | adhocc > tmp/flori.h
%.o: %.c tmp flori.h
	@cp $< tmp/$(notdir $<)
	$(CC) $(CFLAGS) -c tmp/$(notdir $<)

flori: bin $(FLORI_LIBS) flori.o flori.h
	$(CC) $(CFLAGS) -o bin/flori $(FLORI_LIBS) flori.o
lexertest.out: $(FLORI_LIBS) test/lexer_test.o flori.h
	$(CC) $(CFLAGS) -o lexertest.out $(FLORI_LIBS) lexer_test.o

test: flori lexertest.out
	./test.sh

clean:
	@rm -rf bin/ *.out *.asm *.o adhocctmp/ tmp/ tostring.c

