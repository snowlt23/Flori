CC = gcc
CFLAGS = -Wall

FLORI_LIBS = string.o vector.o lexer.o parser.o codegen.o flori.h.o

build: flori ;

bin:
	@mkdir bin
tmp:
	@mkdir tmp
adhocctmp/adhocc.out: adhoccfile.c
	adhocc build adhoccfile.c

flori-header: flori.h tmp adhocctmp/adhocc.out
	cat flori.h | adhocc tmp/flori.h.c > tmp/flori.h
flori.h.o: flori-header
	$(CC) $(CFLAGS) -c tmp/flori.h.c
%.o: %.c tmp flori-header
	cat $< | adhocc > tmp/$(notdir $<)
	$(CC) $(CFLAGS) -c tmp/$(notdir $<)

flori: bin flori-header $(FLORI_LIBS) flori.o
	$(CC) $(CFLAGS) -o bin/flori $(FLORI_LIBS) flori.o
lexertest.out: flori-header $(FLORI_LIBS) test/lexer_test.o
	$(CC) $(CFLAGS) -o lexertest.out $(FLORI_LIBS) lexer_test.o
parsertest.out: flori-header $(FLORI_LIBS) test/parser_test.o
	$(CC) $(CFLAGS) -o parsertest.out $(FLORI_LIBS) parser_test.o

test: flori lexertest.out parsertest.out
	./test.sh

clean:
	@rm -rf bin/ *.out *.asm *.o adhocctmp/ tmp/ tostring.c

