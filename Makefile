CC = gcc
CFLAGS = -Wall

FLORI_SRCS = string linmem jit data reloc istring decls fmap parser macrocaller boot flori.h elfgen
FLORI_OBJS = $(FLORI_SRCS:%=tmp/%.o)

build: bin/flori ;

adhocctmp/adhocc.out: adhoccfile.c
	adhocc build adhoccfile.c

tmp/flori.h: flori.h adhocctmp/adhocc.out
	cat flori.h | adhocc tmp/flori.h.c > tmp/flori.h
tmp/flori.h.o: tmp/flori.h
	$(CC) $(CFLAGS) -c tmp/flori.h.c -o tmp/flori.h.o
tmp/%.o: %.c tmp/flori.h
	cat $< | adhocc > tmp/$(notdir $<)
	$(CC) $(CFLAGS) -c tmp/$(notdir $<) -o $@

bin/flori: tmp/flori.h $(FLORI_OBJS) tmp/flori.o
	$(CC) $(CFLAGS) -o bin/flori $(FLORI_OBJS) tmp/flori.o
%.out: tmp/%.o tmp/flori.h $(FLORI_OBJS)
	$(CC) $(CFLAGS) -o $(notdir $(basename $<)).out $(FLORI_OBJS) $<
test: bin/flori
	./test.sh

clean:
	@rm -rf bin/ *.out adhocctmp/ tmp/
	@mkdir bin tmp
	@touch bin/empty.txt tmp/empty.txt
