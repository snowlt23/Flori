CC = gcc
CFLAGS = -Wall

FLORI_LIBS = linmem.o jit.o data.o reloc.o istring.o parser.o semantic.o codegen.o flori.h.o elfgen.o

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
%.out: %.o flori-header $(FLORI_LIBS)
	$(CC) $(CFLAGS) -o $(basename $<).out $(FLORI_LIBS) $(basename $<).o
test: flori
	./test.sh

clean:
	@rm -rf bin/ *.out *.asm *.o adhocctmp/ tmp/ tostring.c
